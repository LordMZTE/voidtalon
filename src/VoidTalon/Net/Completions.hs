{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module VoidTalon.Net.Completions
  ( perform,
    Context (..),
    Update (..),
    TokenStats (..),
    emptyStats,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (finally)
import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (ord)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word8)
import Lens.Micro
import Network.HTTP.Client
  ( BodyReader,
    Manager,
    Request (method, requestBody, requestHeaders),
    RequestBody (RequestBodyLBS),
    Response (responseBody),
    requestFromURI,
    withResponse,
  )
import Network.URI.Lens (uriPathLens)
import System.FilePath ((</>))
import VoidTalon.Config (ConnectionConfig (..), getHeaders)
import VoidTalon.JSON (ToJSONEncoding (..), (.:<>))
import VoidTalon.Net (checkStatusOK)
import VoidTalon.Timeline (LLMMessage (..))
import qualified VoidTalon.Timeline as Timeline
import qualified VoidTalon.Tools as Tools
import VoidTalon.Util (untab)

perform ::
  -- | Consumer that will be called with incoming updates
  (Update -> IO ()) ->
  -- | Action to be executed after everything else finished
  (IO ()) ->
  -- | Connection config
  ConnectionConfig ->
  -- | HTTP connection Manager
  Manager ->
  -- | Context to send the request with
  Context ->
  IO ThreadId
perform evchan done conf http ctx = do
  let endpoint = conf.base_url & uriPathLens %~ (</> "chat/completions")
  req' <- requestFromURI endpoint
  let req =
        req'
          { method = "POST",
            requestBody = RequestBodyLBS $ mkCtxRequestBody ctx,
            requestHeaders = getHeaders conf.headers
          }
  forkIO $ (withResponse req http handleResponse) `finally` done
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
    mkCtxRequestBody = encodingToLazyByteString . toEncoding

    updateFromRaw :: RawUpdate -> IO ()
    updateFromRaw RawUpdate {choices, stats} =
      sequence_ $
        choices <&> \ch -> do
          fromMaybe (pure ()) $ (evchan . flip UpdateMessage stats) <$> ch.message
          fromMaybe (pure ()) $ (evchan . UpdateStop) <$> ch.stop

data Context = Context
  { -- | Model to use
    model :: T.Text,
    timeline :: [Timeline.Entry],
    tools :: [(T.Text, Tools.Description)]
  }

instance ToJSONEncoding Context where
  toEncoding Context {model, timeline, tools} =
    pairs $
      mconcat
        [ "stream" .= True,
          "model" .= model,
          -- This will make the last event include statistics about the token count.  This is part
          -- of the OpenAI API.
          pair "stream_options" (pairs $ "include_usage" .= True),
          -- This is regarding token timings - a llama.cpp extension that also supersedes the
          -- include_usage setting above, but we use that as a fallback.
          "timings_per_token" .= True,
          pair "messages" (list encodeEntry timeline),
          pair "tools" (list encodeTool tools)
        ]
    where
      encodeEntry (Timeline.PromptEntry p) = pairs ("role" .= ("user" :: T.Text) <> "content" .= p)
      encodeEntry (Timeline.OutputEntry (LLMMessage {reasoning, content, toolCalls})) =
        pairs $
          mconcat
            [ "role" .= ("user" :: T.Text),
              "reasoning_content" .= reasoning,
              "content" .= content,
              pair "tool_calls" $ list encodeToolCall (IntMap.elems toolCalls)
            ]
      encodeEntry (Timeline.ToolResultEntry {id = id', content}) =
        pairs $
          mconcat $
            case id' of
              Just id'' -> ["tool_call_id" .= id'']
              Nothing -> []
              ++ ["role" .= ("tool" :: T.Text), "content" .= content]

      encodeTool (name, tool) = toEncoding $ Tools.NamedDescription name tool
      encodeToolCall Tools.Call {id = id', name, parameters} =
        pairs $
          mconcat
            ( case id' of
                Nothing -> []
                Just id'' -> ["id" .= id'']
                ++ [ "type" .= ("function" :: T.Text),
                     pair "function" (pairs $ "arguments" .= parameters <> "name" .= name)
                   ]
            )

data Update
  = UpdateMessage {delta :: LLMMessage, stats :: TokenStats}
  | UpdateStop {reason :: T.Text}

data TokenStats = TokenStats
  { -- | Number of tokens in the prompt
    nPrompt :: Word,
    -- | Number of tokens generated
    nCompletion :: Word,
    -- | Tokens per second (llama.cpp only).  This must be non-negative (because otherwise, we don't
    -- hold up the neutral element monoid law)
    tps :: Float
  }
  deriving (Eq)

-- | Stats to be used when the actual data is unknown
emptyStats :: TokenStats
emptyStats = TokenStats {nPrompt = 0, nCompletion = 0, tps = 0.0}

-- | @TokenStats@ is a Semigroup where the associative operation simply returns the second stats,
-- unless those are empty, indicating the API didn't report them.
-- This reflects the fact that we assume the second argument to be a more recent update than the
-- first.
instance Semigroup TokenStats where
  a <> b | b == emptyStats = a
  _ <> b = b

-- | Parse stats from the "usage" object returned by the OAI API
parseStatsOAI :: Object -> Parser TokenStats
parseStatsOAI v =
  TokenStats
    <$> (v .: "prompt_tokens")
    <*> (v .: "completion_tokens")
    <*> (pure 0) -- tps isn't known

-- | Parse stats from the superior "timings" object returned by Llama.cpp
parseStatsLlamaCpp :: Object -> Parser TokenStats
parseStatsLlamaCpp v =
  TokenStats
    -- These are given as the number of tokens that has been cached and the rest that was processed
    -- for this request.  We could consider reporting these individually, but for now, we just sum
    -- up.
    <$> (liftA2 (+) (v .: "cache_n") (v .: "prompt_n"))
    <*> (v .: "predicted_n")
    <*> (v .: "predicted_per_second")

data RawUpdate = RawUpdate
  { choices :: [Choice],
    stats :: TokenStats
  }

instance FromJSON RawUpdate where
  parseJSON (Object v) =
    RawUpdate
      <$> v .: "choices"
      <*> (
            -- try to parse Llama.cpp timings first
            (v .: "timings" >>= parseStatsLlamaCpp)
              -- ...fall back to OAI metrics
              <|> (v .: "usage" >>= parseStatsOAI)
              -- if all else fails, use empty stats
              <|> pure emptyStats
          )
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
                        <$> (untab <$> d .:<> "reasoning_content")
                        <*> (untab <$> d .:<> "content")
                        <*> ( mconcat
                                <$> (d .:<> "tool_calls" >>= (sequence . fmap parseTools))
                            )
                  )
          )
    where
      parseTools :: Object -> Parser (IntMap.IntMap Tools.Call)
      parseTools t = do
        fun <- t .: "function"
        IntMap.singleton
          <$> (t .: "index")
          <*> ( Tools.Call
                  <$> t .:? "id"
                  -- We treat the name like the arguments - starts empty and is appended onto.  God
                  -- knows if this is how the API is meant to be understood, but it works with a
                  -- well-behaved server anyways.
                  <*> (untab <$> fun .:<> "name")
                  -- Untabbing this might become an issue.  We'll deal with it when it does.
                  <*> (untab <$> fun .:<> "arguments")
              )
  parseJSON _ = mempty
