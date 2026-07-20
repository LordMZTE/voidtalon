{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Tools.ReadFile (tool) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import VoidTalon.Tools

description :: Description
description =
  Description
    { description = "Read a file given a path",
      schema =
        SchemaObject
          { properties = [("path", SchemaString "Path of the file to read")],
            required = ["path"]
          }
    }

newtype Parameters = Parameters FilePath

instance FromJSON Parameters where
  parseJSON (Object v) =
    Parameters <$> (v .: "path")
  parseJSON _ = mempty

invoke :: Value -> Result Invocation
invoke val = do
  Parameters path <- fromJSON val
  pure ([("Path", T.pack path)], perform path)
  where
    perform = LTIO.readFile

tool :: Tool
tool = Tool {description, invoke}
