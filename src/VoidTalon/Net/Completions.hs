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
import Data.Aeson
import Data.Aeson.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (ord)
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
import System.Directory.Internal.Prelude (fromMaybe)
import System.FilePath ((</>))
import VoidTalon.Net (checkStatusOK)
import VoidTalon.Timeline (LLMMessage (..))
import qualified VoidTalon.Timeline as Timeline
import VoidTalon.Util (untab)

perform ::
  -- | Consumer that will be called with incoming updates
  (Update -> IO ()) ->
  -- | API base URL
  URI ->
  -- | HTTP connection Manager
  Manager ->
  -- | Context to send the request with
  Context ->
  IO ()
perform evchan uri http ctx = do
  let endpoint = uri & uriPathLens %~ (</> "chat/completions")
  req' <- requestFromURI endpoint
  let req = req' {method = "POST", requestBody = RequestBodyLBS $ mkCtxRequestBody ctx}
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
        Just p -> updateFromRaw p
        Nothing -> pure () -- parse error
    processLine _ = pure () -- garbage
    handleResponse :: Response BodyReader -> IO ()
    handleResponse res =
      checkStatusOK res >> takeLines mempty res.responseBody

    mkCtxRequestBody :: Context -> LBS.ByteString
    mkCtxRequestBody = encodingToLazyByteString . contextEncoding

    updateFromRaw :: RawUpdate -> IO ()
    updateFromRaw RawUpdate {choices} =
      sequence_ $
        choices <&> \ch -> do
          fromMaybe (pure ()) $ (evchan . UpdateMessage) <$> ch.message
          fromMaybe (pure ()) $ (evchan . UpdateStop) <$> ch.stop

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

data Update
  = UpdateMessage {delta :: LLMMessage}
  | UpdateStop {reason :: T.Text}

data RawUpdate = RawUpdate
  { choices :: [Choice]
  }

instance FromJSON RawUpdate where
  parseJSON (Object v) =
    RawUpdate
      <$> v .: "choices"
  parseJSON _ = mempty

data Choice = Choice {stop :: Maybe T.Text, message :: Maybe LLMMessage}

instance FromJSON Choice where
  parseJSON (Object v) = do
    Choice
      <$> ((untab <$>) <$> v .:? "finish_reason")
      <*> ( v .:? "delta"
              >>= sequence
                . fmap
                  ( \d ->
                      LLMMessage
                        <$> (untab <$> d .:? "reasoning_content" .!= "")
                        <*> (untab <$> d .:? "content" .!= "")
                  )
          )
  parseJSON _ = mempty
