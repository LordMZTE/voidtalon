{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Config (Config (..), ConnectionConfig (..), parseConfig, findDefaultPath) where

import Data.Text (Text)
import GHC.Generics
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import System.FilePath ((</>))
import Toml
import Toml.Schema

data Config = Config
  { connection :: ConnectionConfig
  }
  deriving (Generic)
  deriving (FromValue) via GenericTomlTable Config

data ConnectionConfig = ConnectionConfig
  { base_url :: String
  }
  deriving (Generic)
  deriving (FromValue) via GenericTomlTable ConnectionConfig

parseConfig :: Text -> Result String Config
parseConfig = decode

findDefaultPath :: IO FilePath
findDefaultPath =
  (</> "config.toml") <$> getXdgDirectory XdgConfig "voidtalon"
