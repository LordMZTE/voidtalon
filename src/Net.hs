{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Net (Context (..), perform) where

import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef)
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.TH
import Network.HTTP.Client
  ( BodyReader,
    Manager,
    Request (method, requestBody),
    RequestBody (RequestBodyBS),
    Response,
    requestFromURI,
    withResponse,
  )
import Network.URI (URI)
import Network.URI.Lens (uriPathLens)
import Brick.BChan (BChan)
import TUI (Event)

data Context = Context
  { prompt :: T.Text
  }

perform :: BChan Event -> URI -> Manager -> Context -> IORef Bool -> IO ()
perform evchan uri http ctx running = do
  let endpoint = uri & uriPathLens %~ (<> "/chat/completions")
  req' <- requestFromURI endpoint
  let req = req' {method = "POST", requestBody = RequestBodyBS $ mkCtxRequestBody ctx}
  _ <- forkIO $ withResponse req http $ handleResponse evchan running
  pure ()
  where
    handleResponse :: BChan Event -> IORef Bool -> Response BodyReader -> IO ()
    handleResponse evchan running res = do
      pure ()

mkCtxRequestBody :: Context -> ByteString
mkCtxRequestBody = undefined
