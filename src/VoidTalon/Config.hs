{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module VoidTalon.Config
  ( Config (..),
    TomlURI (..),
    ConnectionConfig (..),
    parseConfig,
    findDefaultPath,
    getHeaders,
  )
where

import Data.Bifunctor (bimap)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI, parseURI)
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import System.FilePath ((</>))
import Toml
import Toml.Schema
import Toml.Schema.FromValue (typeError)

data Config = Config
  { connection :: ConnectionConfig
  }
  deriving (Generic)
  deriving (FromValue) via GenericTomlTable Config

newtype TomlURI = TomlURI {inner :: URI}

instance FromValue TomlURI where
  fromValue (Text' ann txt) = case parseURI $ T.unpack txt of
    Just uri -> pure $ TomlURI uri
    Nothing -> failAt ann "invalid URL"
  fromValue v = typeError "URI" v

data ConnectionConfig = ConnectionConfig
  { base_url :: TomlURI,
    headers :: Map.Map T.Text T.Text
  }
  deriving (Generic)
  deriving (FromValue) via GenericTomlTable ConnectionConfig

parseConfig :: T.Text -> Result String Config
parseConfig = decode

findDefaultPath :: IO FilePath
findDefaultPath =
  (</> "config.toml") <$> getXdgDirectory XdgConfig "voidtalon"

getHeaders :: Map.Map T.Text T.Text -> HTTP.RequestHeaders
getHeaders = fmap (bimap (CI.mk . T.encodeUtf8) T.encodeUtf8) . Map.toList
