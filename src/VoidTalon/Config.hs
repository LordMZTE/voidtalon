{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Config
  ( Config (..),
    ConnectionConfig (..),
    parseConfig,
    fileName,
    findDefaultDir,
    getHeaders,
    module VoidTalon.Config.Example,
  )
where

import Data.Bifunctor (bimap)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vec
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI, parseURI)
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import Toml
import Toml.Schema
import Toml.Schema.FromValue (typeError)
import VoidTalon.Config.Example
import VoidTalon.Util (parseTomlVector)

data Config = Config
  { connections :: (Vec.Vector ConnectionConfig)
  }

instance FromValue Config where
  fromValue =
    parseTableFromValue $
      Config
        <$> reqKeyOf "connections" parseTomlVector

data ConnectionConfig = ConnectionConfig
  { name :: T.Text,
    base_url :: URI,
    headers :: Map.Map T.Text T.Text,
    defaultModel :: Maybe T.Text
  }
  deriving (Eq)

instance FromValue ConnectionConfig where
  fromValue =
    parseTableFromValue $
      ConnectionConfig
        <$> reqKey "name"
        <*> reqKeyOf "base_url" parseTomlURI
        <*> (fromMaybe mempty <$> optKey "headers")
        <*> optKey "default_model"
    where
      parseTomlURI (Text' ann txt) = case parseURI $ T.unpack txt of
        Just uri -> pure $ uri
        Nothing -> failAt ann "invalid URL"
      parseTomlURI v = typeError "URI" v

parseConfig :: T.Text -> Result String Config
parseConfig = decode

-- | Finds the default configuration directory if the user didn't explicitly specify it on the
-- command line
findDefaultDir :: IO FilePath
findDefaultDir = getXdgDirectory XdgConfig "voidtalon"

-- | Name of the config file in the config directory
fileName :: FilePath
fileName = "config.toml"

getHeaders :: Map.Map T.Text T.Text -> HTTP.RequestHeaders
getHeaders = fmap (bimap (CI.mk . T.encodeUtf8) T.encodeUtf8) . Map.toList
