{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module VoidTalon.Net.Completions
  ( perform,
    Context (..),
    contextEncoding,
    Update (..),
  )
where

import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Data.Aeson
import Data.Aeson.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (ord)
import Data.IORef (IORef, atomicWriteIORef)
import qualified Data.Text as T
import Data.Word (Word8)
import Lens.Micro
import Network.HTTP.Client
  ( BodyReader,
    Manager,
    Request (method, requestBody),
    RequestBody (RequestBodyLBS),
    Response (responseBody),
    requestFromURI,
    withResponse,
  )
import Network.URI (URI)
import Network.URI.Lens (uriPathLens)
import System.FilePath ((</>))
import VoidTalon.Net (checkStatusOK)
import VoidTalon.Timeline (LLMMessage (..))
import qualified VoidTalon.Timeline as Timeline

perform ::
  -- | Consumer that will be called with incoming updates
  (Update -> IO ()) ->
  -- | API base URL
  URI ->
  -- | HTTP connection Manager
  Manager ->
  -- | The value will be set to true to indicate an ongoing operation and set to false once we're done.
  IORef Bool ->
  -- | Context to send the request with
  Context ->
  IO ()
perform evchan uri http running ctx = do
  let endpoint = uri & uriPathLens %~ (</> "chat/completions")
  req' <- requestFromURI endpoint
  let req = req' {method = "POST", requestBody = RequestBodyLBS $ mkCtxRequestBody ctx}
  atomicWriteIORef running True
  _ <- forkIO $ withResponse req http handleResponse
  pure ()
  where
    foldChar :: IO BSB.Builder -> Word8 -> IO BSB.Builder

    -- Hit newline, consume buffer and start with new empty buffer
    foldChar prev ch | ch == (fromIntegral $ ord '\n') = do
      line <- prev
      processLine $ BSB.toLazyByteString line
      pure mempty

    -- Not newline, append to buffer
    foldChar prev ch = (<> BSB.word8 ch) <$> prev

    takeLines :: BSB.Builder -> BodyReader -> IO ()
    takeLines buf res = do
      chunk <- res
      if chunk == ""
        then pure ()
        else do
          buf' <- BS.foldl' foldChar (pure buf) chunk
          takeLines buf' res

    processLine :: LBS.ByteString -> IO ()
    processLine l | l LBS.!? 0 == Just ':' = pure () -- comment
    processLine "data: [DONE]" = pure () -- completely useless terminator line
    processLine (LBS.stripPrefix "data:" -> Just l) = do
      case decode l of
        Just p -> evchan $ updateFromRaw p
        Nothing -> pure () -- parse error
    processLine _ = pure () -- garbage
    handleResponse :: Response BodyReader -> IO ()
    handleResponse res =
      finally
        (checkStatusOK res >> takeLines mempty res.responseBody)
        (atomicWriteIORef running False)

    mkCtxRequestBody :: Context -> LBS.ByteString
    mkCtxRequestBody = encodingToLazyByteString . contextEncoding

data Context = Context
  { -- | Model to use
    model :: T.Text,
    timeline :: [Timeline.Entry]
  }

-- We don't specify a ToJSON instance because that would require us to also implement `toJSON`,
-- causing code duplication and no advantage
contextEncoding :: Context -> Encoding
contextEncoding Context {model, timeline} =
  pairs
    ( "stream" .= True
        <> "model" .= model
        <> pair
          ("messages" :: Key)
          ( list
              id
              $ encodeEntry <$> timeline
              -- [ pairs ("role" .= ("user" :: T.Text) <> "content" .= prompt)
              -- ]
          )
    )
  where
    encodeEntry (Timeline.PromptEntry p) = pairs ("role" .= ("user" :: T.Text) <> "content" .= p)
    encodeEntry (Timeline.OutputEntry (LLMMessage {reasoning, content})) =
      pairs
        ( "role" .= ("user" :: T.Text)
            <> "reasoning_content" .= reasoning
            <> "content" .= content
        )

data Update = Update
  { delta :: LLMMessage
  }

updateFromRaw :: RawUpdate -> Update
updateFromRaw RawUpdate {choices} = Update {delta = mconcat $ (.inner) <$> choices}

data RawUpdate = RawUpdate
  { choices :: [CompletionChoice]
  }

instance FromJSON RawUpdate where
  parseJSON (Object v) =
    RawUpdate
      <$> v .: "choices"
  parseJSON _ = mempty

newtype CompletionChoice = CompletionChoice {inner :: LLMMessage}

instance FromJSON CompletionChoice where
  parseJSON (Object v) = do
    d <- v .: "delta"
    CompletionChoice
      <$> ( LLMMessage
              <$> d .:? "reasoning_content" .!= ""
              <*> d .:? "content" .!= ""
          )
  parseJSON _ = mempty
