{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Tools.BuiltIn.ReadFile (tool) where

import Data.Aeson hiding (toEncoding)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import VoidTalon.JSON
  ( Schema (..),
    SchemaType (SchemaTypeObject, SchemaTypeString),
    emptySchema,
  )
import VoidTalon.Tools

description :: Description
description =
  Description
    { description = "Read a file given a path",
      schema =
        Left $
          emptySchema
            { types = [SchemaTypeObject],
              properties =
                [ ( "path",
                    emptySchema
                      { types = [SchemaTypeString],
                        description = Just "Path of the file to read"
                      }
                  )
                ],
              required = ["path"]
            }
    }

newtype Parameters = Parameters FilePath

instance FromJSON Parameters where
  parseJSON = withObject "Parameters" $ \v ->
    Parameters <$> v .: "path"

invoke :: T.Text -> Either String Invocation
invoke val = do
  Parameters path <- eitherDecodeStrictText val
  pure ([("Path", T.pack path)], perform path)
  where
    perform =
      fmap
        ( \case
            c | T.null c -> "<empty file>"
            c | T.all isSpace c -> "<only whitespace>"
            c -> c
            . T.decodeUtf8
        )
        . BS.readFile

tool :: Tool
tool = Tool {description, invoke}
