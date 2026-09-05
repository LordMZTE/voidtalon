{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Tools.RunCommand (tool) where

import Data.Aeson
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Exit (ExitCode (..))
import System.IO (Handle, stdout)
import System.Process
  ( CreateProcess (..),
    StdStream (CreatePipe, UseHandle),
    createProcess,
    shell,
    waitForProcess,
  )
import VoidTalon.JSON (Schema (..), SchemaType (SchemaTypeObject, SchemaTypeString), emptySchema)
import VoidTalon.Tools

description :: Description
description =
  Description
    { description =
        "Run a command in a POSIX-compliant shell\n"
          <> "Note that shell state (i.e. environment variables) is **NOT** preserved across tool calls.",
      schema =
        Left $
          emptySchema
            { types = [SchemaTypeObject],
              properties =
                [ ( "command",
                    emptySchema
                      { types = [SchemaTypeString],
                        description = Just "The command to run"
                      }
                  )
                ],
              required = ["command"]
            }
    }

newtype Parameters = Parameters T.Text

instance FromJSON Parameters where
  parseJSON = withObject "Parameters" $ \v ->
    Parameters <$> v .: "command"

invoke :: T.Text -> Either String Invocation
invoke val = do
  Parameters command <- eitherDecodeStrictText val
  pure ([("Command", command)], perform command)
  where
    perform command = do
      -- This is a cool trick to bind stdout and stderr to the same pipe.
      let spec =
            (shell (T.unpack command))
              { std_out = CreatePipe,
                std_err = UseHandle stdout,
                delegate_ctlc = True
              }
      (Nothing, Just out, Nothing, pid) <- createProcess spec
      output <- consumeAndEcho out
      exit <- waitForProcess pid

      let output' = case T.decodeUtf8Lenient . LBS8.toStrict . BSB.toLazyByteString $ output of
            o | T.null o -> "<no output>"
            o | T.all isSpace o -> "<only whitespace in output>"
            o -> o

      pure $ case exit of
        ExitSuccess -> output'
        ExitFailure n ->
          mconcat
            [ "Warn: Process exited with nonzero code ",
              T.show n,
              "\n",
              output'
            ]
    consumeAndEcho :: Handle -> IO BSB.Builder
    consumeAndEcho h = do
      output <- BS8.hGetSome h (8 * 1024)
      if BS8.null output
        then pure mempty
        else do
          BS8.putStr output
          let output' = BSB.byteString output
          more <- consumeAndEcho h
          pure $ output' <> more

tool :: Tool
tool = Tool {description, invoke}
