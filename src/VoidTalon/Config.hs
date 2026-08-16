{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Config
  ( Config (..),
    ConnectionConfig (..),
    ModelConfig (..),
    parseConfig,
    findDefaultPath,
    getHeaders,
  )
where

import Data.Bifunctor (bimap)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI, parseURI)
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import System.FilePath ((</>))
import Toml
import Toml.Schema
import Toml.Schema.FromValue (typeError)

data Config = Config
  { connection :: ConnectionConfig,
    model :: ModelConfig
  }

instance FromValue Config where
  fromValue =
    parseTableFromValue $
      Config
        <$> reqKey "connection"
        <*> (fromMaybe defaultModelConfig <$> optKey "model")

data ConnectionConfig = ConnectionConfig
  { base_url :: URI,
    headers :: Map.Map T.Text T.Text
  }

instance FromValue ConnectionConfig where
  fromValue =
    parseTableFromValue $
      ConnectionConfig
        <$> reqKeyOf "base_url" parseTomlURI
        <*> (fromMaybe mempty <$> optKey "headers")
    where
      parseTomlURI (Text' ann txt) = case parseURI $ T.unpack txt of
        Just uri -> pure $ uri
        Nothing -> failAt ann "invalid URL"
      parseTomlURI v = typeError "URI" v

data ModelConfig = ModelConfig
  { standard :: Maybe T.Text
  }

instance FromValue ModelConfig where
  fromValue = parseTableFromValue $ ModelConfig <$> optKey "standard"

defaultModelConfig :: ModelConfig
defaultModelConfig = ModelConfig {standard = Nothing}

parseConfig :: T.Text -> Result String Config
parseConfig = decode

findDefaultPath :: IO FilePath
findDefaultPath =
  (</> "config.toml") <$> getXdgDirectory XdgConfig "voidtalon"

getHeaders :: Map.Map T.Text T.Text -> HTTP.RequestHeaders
getHeaders = fmap (bimap (CI.mk . T.encodeUtf8) T.encodeUtf8) . Map.toList
