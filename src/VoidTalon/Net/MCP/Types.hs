{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Net.MCP.Types
  ( methodInitialize,
    methodToolsList,
    methodToolsCall,
    methodNotifInitialized,
    hSessionID,
    JSONRPCMessage (..),
    JSONRPCServerMessage (..),
    JSONRPCReply (..),
    JSONRPCEvent (..),
    ServerCapabilities (..),
    ServerInfo (..),
    InitializeReply (..),
    RPCFailure (..),
    InitFailure (..),
    ToolSpec (..),
    ToolListReply (..),
    ToolCallReply (..),
    HeaderMap,
    HTTPConnectionSpec (..),
  )
where

import Control.Exception (Exception)
import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding
import Data.Aeson.KeyMap (member)
import Data.Aeson.Types (Parser)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI, parseURI)
import VoidTalon.JSON (ToJSONEncoding (toEncoding))

methodInitialize :: T.Text
methodInitialize = "initialize"

methodToolsList :: T.Text
methodToolsList = "tools/list"

methodToolsCall :: T.Text
methodToolsCall = "tools/call"

methodNotifInitialized :: T.Text
methodNotifInitialized = "notifications/initialized"

hSessionID :: (IsString a) => a
hSessionID = "MCP-Session-Id"

data JSONRPCMessage = JSONRPCMessage {id :: Maybe Int, method :: T.Text, params :: Encoding}

instance ToJSONEncoding JSONRPCMessage where
  toEncoding JSONRPCMessage {id = id', method, params} =
    pairs
      . mconcat
      $ maybeToList (("id" .=) <$> id')
        ++ [ "jsonrpc" .= ("2.0" :: T.Text),
             "method" .= method,
             pair "params" params
           ]

data JSONRPCServerMessage r
  = JSONRPCServerReply (JSONRPCReply r)
  | JSONRPCServerEvent JSONRPCEvent

instance (FromJSON r) => FromJSON (JSONRPCServerMessage r) where
  parseJSON obj =
    withObject
      "JSONRPCServerMessage"
      ( \v ->
          if member "id" v
            then JSONRPCServerReply <$> parseJSON obj
            else JSONRPCServerEvent <$> parseJSON obj
      )
      obj

data JSONRPCReply r = JSONRPCReply {id :: Int, result :: r}

instance (FromJSON r) => FromJSON (JSONRPCReply r) where
  parseJSON = withObject "JSONRPCReply" $ \v ->
    JSONRPCReply
      <$> v .: "id"
      <*> v .: "result"

-- | An event such as a notification sent by an MCP server while we were waiting for a response.
data JSONRPCEvent = JSONRPCEvent {method :: T.Text, params :: Value}

instance FromJSON JSONRPCEvent where
  parseJSON = withObject "JSONRPCEvent" $ \v ->
    JSONRPCEvent
      <$> v .: "method"
      <*> v .: "params"

data ServerCapabilities = ServerCapabilities {tools :: Bool}

instance FromJSON ServerCapabilities where
  parseJSON = withObject "ServerCapabilities" $ \v ->
    pure $ ServerCapabilities $ member "tools" v

data ServerInfo = ServerInfo
  { name :: T.Text,
    title :: Maybe T.Text
  }

instance FromJSON ServerInfo where
  parseJSON = withObject "ServerInfo" $ \v ->
    ServerInfo <$> v .: "name" <*> v .:? "title"

data InitializeReply = InitializeReply
  { capabilities :: ServerCapabilities,
    instructions :: Maybe T.Text,
    serverInfo :: ServerInfo
  }

instance FromJSON InitializeReply where
  parseJSON = withObject "InitializeReply" $ \v ->
    InitializeReply
      <$> v .: "capabilities"
      <*> v .:? "instructions"
      <*> v .: "serverInfo"

data RPCFailure
  = -- | Could not decode JSON from server
    RPCFailureDecode String
  | -- | Server responded with bad message ID
    RPCFailureIDMismatch
  | -- | Server sent no response to method call or only unrelated events
    RPCFailureNoResponse
  deriving (Show)

instance Exception RPCFailure

data InitFailure
  = -- | Server does not have to tools capability
    InitFailureNoTools
  deriving (Show)

instance Exception InitFailure

-- | Specification for a tool returned from the MCP server
data ToolSpec = ToolSpec
  { name :: T.Text,
    description :: T.Text,
    -- | You may be asking yourself why this isn't a `Schema`, but an untyped Value. Well, the
    -- reason is that whoever invented json-schema was a complete nut job!  The whole format is a
    -- dumpster fire, and a spec-complaint parser would probably make up the majority of this code
    -- base if I were willing to sacrifice enough of my sanity to implement one.  Clearly, JSON
    -- schemas are more suitable as human-readable (or AI-readable) data types than machine-readable
    -- ones.
    inputSchema :: Value
  }

instance FromJSON ToolSpec where
  parseJSON = withObject "ToolSpec" $ \v ->
    ToolSpec
      <$> v .: "name"
      <*> v .: "description"
      <*> v .: "inputSchema"

-- | Server reply to "tools/list".
data ToolListReply = ToolListReply {nextCursor :: Maybe Value, tools :: [ToolSpec]}

instance FromJSON ToolListReply where
  parseJSON = withObject "ToolListReply" $ \v ->
    ToolListReply
      <$> v .:? "nextCursor"
      <*> v .: "tools"

newtype ToolCallReply = ToolCallReply T.Text

instance FromJSON ToolCallReply where
  parseJSON = withObject "ToolCallReply" $ \v -> do
    cs <- v .: "content" :: Parser [Object]
    ls <- sequence $ parseContent <$> cs
    pure $ ToolCallReply $ T.unlines ls
    where
      parseContent c = do
        ty <- c .: "type" :: Parser T.Text
        case ty of
          "text" -> c .: "text"
          "image" -> pure "[tool responded with image, this is currently unsupported]" -- TODO
          "audio" -> pure "[tool responded with audio, this is currently unsupported]" -- TODO
          "resource" ->
            -- TODO: we should probably fetch this resource in this case and forward it to the LLM
            pure "[tool responded with resource, this is currently unsupported]"
          x -> pure $ mconcat ["[content of unknown type '", x, "']"]

type HeaderMap = Map.Map T.Text T.Text

data HTTPConnectionSpec = HTTPConnectionSpec
  { uri :: URI,
    headers :: [HTTP.Header]
  }

instance Read HTTPConnectionSpec where
  readsPrec _ s = maybeToList $ do
    let t = T.pack s
    -- Using `<-` instead of `let` to handle non-exhaustive pattern
    uri' : headers' <- Just $ T.split ('|' ==) t
    uri <- parseURI $ T.unpack uri'
    headers <- mapM parseHeaderKV headers'
    Just (HTTPConnectionSpec {uri, headers}, [])
    where
      parseHeaderKV :: T.Text -> Maybe HTTP.Header
      parseHeaderKV kv = case T.split ('=' ==) kv of
        [k, v] -> Just (CI.mk $ T.encodeUtf8 k, T.encodeUtf8 v)
        _ -> Nothing
