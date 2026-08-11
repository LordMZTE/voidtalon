{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Net.Models (ModelInfo (..), Pricing (..), list) where

import Control.Monad (join)
import Data.Aeson
import Data.Text as T
import Lens.Micro
import Network.HTTP.Client
  ( Manager,
    Request (method, requestHeaders),
    Response (responseBody),
    httpLbs,
    requestFromURI,
  )
import Network.URI.Lens (uriPathLens)
import System.FilePath ((</>))
import VoidTalon.Config (ConnectionConfig (..), TomlURI (..), getHeaders)
import VoidTalon.Net (checkStatusOK)

-- | Information about what model usage costs.  The API sends the prices as strings.
data Pricing = Pricing
  { prompt :: T.Text,
    completion :: T.Text
  }

instance FromJSON Pricing where
  parseJSON (Object v) =
    Pricing
      <$> (v .: "prompt")
      <*> (v .: "completion")
  parseJSON _ = mempty

-- | Information about a model as returned by the /models endpoint.
-- OpenAI only specifies the id, name, and owned_by fields, the rest are extensions commonly found
-- in the wild and on OpenRouter.
data ModelInfo = ModelInfo
  { id :: T.Text,
    name :: Maybe T.Text,
    description :: Maybe T.Text,
    contextLength :: Maybe Int,
    pricing :: Maybe Pricing,
    -- | stored in JSON as reasoning.supported_efforts, where both of those fields are optional
    supportedReasoningEfforts :: Maybe [T.Text]
  }

instance FromJSON ModelInfo where
  parseJSON (Object v) =
    ModelInfo
      <$> (v .: "id")
      <*> (v .:? "name")
      <*> (v .:? "description")
      <*> (v .:? "context_length")
      <*> (v .:? "pricing")
      <*> (v .:? "reasoning" >>= (fmap join . mapM (.:? "supported_efforts")))
  parseJSON _ = mempty

newtype ListResponse = ListResponse [ModelInfo]

instance FromJSON ListResponse where
  parseJSON (Object v) =
    ListResponse
      <$> (v .: "data")
  parseJSON _ = mempty

list ::
  ConnectionConfig ->
  Manager ->
  IO [ModelInfo]
list conf man = do
  let endpoint = conf.base_url.inner & uriPathLens %~ (</> "models")
  req' <- requestFromURI endpoint
  let req = req' {method = "GET", requestHeaders = getHeaders conf.headers}
  res <- httpLbs req man
  checkStatusOK res
  case eitherDecode res.responseBody of
    Left e -> fail $ "/models API returned invalid data: " ++ e
    Right (ListResponse res') -> pure res'
