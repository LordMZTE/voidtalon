{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Tools.WriteFile (tool) where

import Data.Aeson hiding (toEncoding)
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
    { description = "Write text to a file at a given path",
      schema =
        Left $
          emptySchema
            { types = [SchemaTypeObject],
              properties =
                [ ( "path",
                    emptySchema
                      { types = [SchemaTypeString],
                        description = Just "Path of the file to write to"
                      }
                  ),
                  ( "content",
                    emptySchema
                      { types = [SchemaTypeString],
                        description = Just "Content to write to the file"
                      }
                  )
                ],
              required = ["path", "content"]
            }
    }

data Parameters = Parameters FilePath T.Text

instance FromJSON Parameters where
  parseJSON = withObject "Parameters" $ \v ->
    Parameters <$> v .: "path" <*> v .: "content"

invoke :: T.Text -> Either String Invocation
invoke val = do
  Parameters path content <- eitherDecodeStrictText val
  pure ([("Path", T.pack path), ("Content", content)], perform path content)
  where
    perform :: FilePath -> T.Text -> IO T.Text
    perform path content = do
      TIO.writeFile path content
      pure $ mconcat [T.show (T.length content), " chars written to `", T.pack path, "`"]

tool :: Tool
tool = Tool {description, invoke}
