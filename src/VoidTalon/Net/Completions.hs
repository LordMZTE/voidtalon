{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.Net.Completions
  ( perform,
    ReasoningEffort (..),
    Context (..),
    Update (..),
    TokenStats (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
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
import qualified VoidTalon.Net.SSE as SSE
import VoidTalon.Timeline (LLMMessage (..))
import qualified VoidTalon.Timeline as Timeline
import qualified VoidTalon.Tools as Tools
import VoidTalon.Util (untab)

perform ::
  -- | Consumer that will be called with incoming updates
  (Update -> IO ()) ->
  -- | Connection config
  ConnectionConfig ->
  -- | HTTP connection Manager
  Manager ->
  -- | Context to send the request with
  Context ->
  IO ()
perform evchan conf http ctx = do
  let endpoint = conf.base_url & uriPathLens %~ (</> "chat/completions")
  req' <- requestFromURI endpoint
  let req =
        req'
          { method = "POST",
            requestBody = RequestBodyLBS $ mkCtxRequestBody ctx,
            requestHeaders = getHeaders conf.headers
          }
  withResponse req http handleResponse
  where
    handleResponse :: Response BodyReader -> IO ()
    handleResponse res = do
      checkStatusOK res
      SSE.readStream res.responseBody $ \SSE.Event {content} -> do
        if content == "[DONE]"
          then pure ()
          else case decode content of
            Just p -> updateFromRaw p
            Nothing -> pure () -- parse error
    mkCtxRequestBody :: Context -> LBS.ByteString
    mkCtxRequestBody = encodingToLazyByteString . toEncoding

    updateFromRaw :: RawUpdate -> IO ()
    updateFromRaw RawUpdate {choices, stats} =
      sequence_ $
        choices <&> \ch -> do
          maybe (pure ()) (evchan . flip UpdateMessage stats) ch.message
          maybe (pure ()) (evchan . UpdateStop) ch.stop

data ReasoningEffort
  = -- | Don't specify reasoning effort to the API
    REUnspecified
  | -- | Control reasoning effort by token count using llama.cpp's API
    RELlamaCppTokens Word
  | -- | Use a pre-defined effort using the standard API
    REStr T.Text

data Context = Context
  { -- | Model to use
    model :: T.Text,
    timeline :: [Timeline.Entry],
    tools :: [(T.Text, Tools.Description)],
    reasoningEffort :: ReasoningEffort
  }

instance ToJSONEncoding Context where
  toEncoding Context {model, timeline, tools, reasoningEffort} =
    pairs $
      mconcat $
        [ "stream" .= True,
          "model" .= model,
          -- This will make the last event include statistics about the token count.  This is part
          -- of the OpenAI API.
          pair "stream_options" (pairs $ "include_usage" .= True),
          -- This is regarding token timings - a llama.cpp extension that also supersedes the
          -- include_usage setting above, but we use that as a fallback.
          "timings_per_token" .= True,
          -- This enables a llama.cpp extension that makes the API return prompt processing
          -- progress.
          "return_progress" .= True,
          pair "messages" (list encodeEntry timeline),
          pair "tools" (list encodeTool tools)
        ]
          ++ case reasoningEffort of
            REUnspecified -> []
            RELlamaCppTokens n -> ["thinking_budget_tokens" .= n]
            REStr s -> [pair "reasoning" $ pairs $ "effort" .= s]
    where
      encodeEntry (Timeline.SystemEntry p) = pairs ("role" .= ("system" :: T.Text) <> "content" .= p)
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

data TokenStats
  = -- | Server didn't send any usable token statistics yet
    TokenStatsEmpty
  | -- | Prompt processing token statistics, only supported on llama.cpp.
    TokenStatsProcess
      { total :: Word,
        processed :: Word,
        tps :: Float
      }
  | -- | Output generation token statistics.
    TokenStatsGen
      { -- | Number of tokens in the prompt
        nPrompt :: Word,
        -- | Number of tokens generated
        nCompletion :: Word,
        -- | Number of tokens in the context window
        nCtx :: Word,
        -- | Tokens per second (llama.cpp only).  This must be non-negative (because otherwise, we don't
        -- hold up the neutral element monoid law)
        tps :: Float
      }
  deriving (Eq)

-- | @TokenStats@ is a Semigroup where the associative operation simply returns the second stats,
-- unless those are empty, indicating the API didn't report them.  We also prefer token generation
-- stats over prompt processing stats because we don't want to show the latter after we've already
-- started generating, although the API will probably not produce such data anyways.
-- This reflects the fact that we assume the second argument to be a more recent update than the
-- first.
instance Semigroup TokenStats where
  a <> TokenStatsEmpty = a
  a@TokenStatsGen {} <> TokenStatsProcess {} = a -- always prefer gen stats over process stats
  _ <> b = b

parseStatsPromptProcessing :: Object -> Parser TokenStats
parseStatsPromptProcessing v = do
  promptProgress <- v .: "prompt_progress"
  timings <- v .: "timings"
  TokenStatsProcess
    <$> (promptProgress .: "total")
    <*> (promptProgress .: "processed")
    <*> (timings .: "prompt_per_second")

-- | Parse stats from the "usage" object returned by the OAI API
parseStatsOAI :: Object -> Parser TokenStats
parseStatsOAI v =
  TokenStatsGen
    <$> (v .: "prompt_tokens")
    <*> (v .: "completion_tokens")
    <*> (v .: "total_tokens")
    <*> (pure 0) -- tps isn't known

-- | Parse stats from the superior "timings" object returned by Llama.cpp
parseStatsLlamaCpp :: Object -> Parser TokenStats
parseStatsLlamaCpp v = do
  -- These are given as the number of tokens that has been cached and the rest that was processed
  -- for this request.
  promptN <- liftA2 (+) (v .: "cache_n") (v .: "prompt_n")
  predictedN <- v .: "predicted_n"
  TokenStatsGen promptN predictedN (promptN + predictedN)
    <$> (v .: "predicted_per_second")

data RawUpdate = RawUpdate
  { choices :: [Choice],
    stats :: TokenStats
  }

instance FromJSON RawUpdate where
  parseJSON = withObject "RawUpdate" $ \v ->
    RawUpdate
      <$> v .: "choices"
      <*> ( -- try to parse prompt processing timings first
            -- (this has to go first because llama.cpp stats would parse here as well)
            parseStatsPromptProcessing v
              -- ..then try to parse Llama.cpp timings first
              <|> (v .: "timings" >>= parseStatsLlamaCpp)
              -- ...fall back to OAI metrics
              <|> (v .: "usage" >>= parseStatsOAI)
              -- if all else fails, use empty stats
              <|> pure TokenStatsEmpty
          )

data Choice = Choice {stop :: Maybe T.Text, message :: Maybe LLMMessage}

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \v ->
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
          <$> t .: "index"
          <*> ( Tools.Call
                  <$> t .:? "id"
                  -- We treat the name like the arguments - starts empty and is appended onto.  God
                  -- knows if this is how the API is meant to be understood, but it works with a
                  -- well-behaved server anyways.
                  <*> (untab <$> fun .:<> "name")
                  -- Untabbing this might become an issue.  We'll deal with it when it does.
                  <*> (untab <$> fun .:<> "arguments")
              )
