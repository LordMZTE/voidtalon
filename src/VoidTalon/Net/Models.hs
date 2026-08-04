{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Net.Models (ModelInfo (..), list) where

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

data ModelInfo = ModelInfo
  { id :: T.Text,
    -- | Unix Timestamp
    created :: Int
  }

instance FromJSON ModelInfo where
  parseJSON (Object v) =
    ModelInfo
      <$> (v .: "id")
      <*> (v .: "created")
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
