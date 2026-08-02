{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Tools.RunCommand (tool) where

import Data.Aeson
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO8
import System.Exit (ExitCode (..))
import System.Process (StdStream (CreatePipe), createProcess, shell, std_out, waitForProcess)
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
  parseJSON (Object v) = Parameters <$> (v .: "command")
  parseJSON _ = mempty

invoke :: T.Text -> Either String Invocation
invoke val = do
  Parameters command <- eitherDecodeStrictText val
  pure ([("Command", command)], perform command)
  where
    perform command = do
      let spec = (shell (T.unpack command)) {std_out = CreatePipe}
      (Nothing, Just stdout, Nothing, pid) <- createProcess spec
      output <- TIO8.hGetContents stdout
      exit <- waitForProcess pid

      let output' = case output of
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

tool :: Tool
tool = Tool {description, invoke}
