{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Tools.ReadFile (tool) where

import Data.Aeson hiding (toEncoding)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
  parseJSON (Object v) =
    Parameters <$> (v .: "path")
  parseJSON _ = mempty

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
        )
        . TIO.readFile

tool :: Tool
tool = Tool {description, invoke}
