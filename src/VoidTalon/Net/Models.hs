{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Net.Models (ModelInfo (..), list) where

import Data.Aeson
import Data.Text as T
import Lens.Micro
import Network.HTTP.Client
  ( Manager,
    Request (method),
    Response (responseBody),
    httpLbs,
    requestFromURI,
  )
import Network.URI (URI)
import Network.URI.Lens (uriPathLens)
import System.FilePath ((</>))
import VoidTalon.Net (checkStatusOK)

data ModelInfo = ModelInfo
  { id :: T.Text,
    owned_by :: T.Text,
    -- | Unix Timestamp
    created :: Int
  }

instance FromJSON ModelInfo where
  parseJSON (Object v) =
    ModelInfo
      <$> (v .: "id")
      <*> (v .: "owned_by")
      <*> (v .: "created")
  parseJSON _ = mempty

newtype ListResponse = ListResponse [ModelInfo]

instance FromJSON ListResponse where
  parseJSON (Object v) =
    ListResponse
      <$> (v .: "data")
  parseJSON _ = mempty

list ::
  -- | API Base URL
  URI ->
  Manager ->
  IO [ModelInfo]
list base_url man = do
  let endpoint = base_url & uriPathLens %~ (</> "models")
  req' <- requestFromURI endpoint
  let req = req' {method = "GET"}
  res <- httpLbs req man
  checkStatusOK res
  case decode res.responseBody of
    Just (ListResponse res') -> pure res'
    Nothing -> fail "/models API returned invalid data"
