{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Tools.WriteFile (tool) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import VoidTalon.Tools
import VoidTalon.JSON (Schema(..))

description :: Description
description =
  Description
    { description = "Write text to a file at a given path",
      schema =
        SchemaObject
          { properties =
              [ ("path", SchemaString "Path of the file to write to"),
                ("content", SchemaString "Content to write to the file")
              ],
            required = ["path", "content"]
          }
    }

data Parameters = Parameters FilePath T.Text

instance FromJSON Parameters where
  parseJSON (Object v) =
    Parameters <$> (v .: "path") <*> (v .: "content")
  parseJSON _ = mempty

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
