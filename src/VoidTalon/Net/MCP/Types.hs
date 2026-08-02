{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Net.MCP.Types
  ( methodInitialize,
    methodToolsList,
    methodToolsCall,
    JSONRPCMessage (..),
    JSONRPCReply (..),
    ServerCapabilities (..),
    InitializeReply (..),
    RPCFailure (..),
    InitFailure (..),
    ToolSpec (..),
    ToolListReply (..),
    ToolCallReply (..),
  )
where

import Control.Exception (Exception)
import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding
import Data.Aeson.KeyMap (member)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import VoidTalon.JSON (Schema, ToJSONEncoding (toEncoding), (.:<>))

methodInitialize :: T.Text
methodInitialize = "initialize"

methodToolsList :: T.Text
methodToolsList = "tools/list"

methodToolsCall :: T.Text
methodToolsCall = "tools/call"

data JSONRPCMessage = JSONRPCMessage {id :: Maybe Int, method :: T.Text, params :: Encoding}

instance ToJSONEncoding JSONRPCMessage where
  toEncoding JSONRPCMessage {id = id', method, params} =
    pairs $
      mconcat
        [ "jsonrpc" .= ("2.0" :: T.Text),
          "id" .= id',
          "method" .= method,
          pair "params" params
        ]

data JSONRPCReply r = JSONRPCReply {id :: Int, result :: r}

instance (FromJSON r) => FromJSON (JSONRPCReply r) where
  parseJSON (Object v) =
    JSONRPCReply
      <$> (v .: "id")
      <*> (v .: "result")
  parseJSON _ = mempty

data ServerCapabilities = ServerCapabilities {tools :: Bool}

instance FromJSON ServerCapabilities where
  parseJSON (Object v) = pure $ ServerCapabilities $ member "tools" v
  parseJSON _ = mempty

data InitializeReply = InitializeReply
  { capabilities :: ServerCapabilities,
    -- TODO: do something with these instructions.  Maybe add them to the system prompt or
    -- something.  Not sure how these are meant to be used.
    instructions :: T.Text
  }

instance FromJSON InitializeReply where
  parseJSON (Object v) =
    InitializeReply
      <$> (v .: "capabilities")
      <*> (v .:<> "instructions")
  parseJSON _ = mempty

data RPCFailure
  = -- | Could not decode JSON from server
    RPCFailureDecode String
  | -- | Server responded with bad message ID
    RPCFailureIDMismatch
  deriving (Show)

instance Exception RPCFailure

data InitFailure
  = -- | Communication with server Failed
    InitFailureRPC RPCFailure
  | -- | Server does not have to tools capability
    InitFailureNoTools
  deriving (Show)

instance Exception InitFailure

-- | Specification for a tool returned from the MCP server
data ToolSpec = ToolSpec {name :: T.Text, description :: T.Text, inputSchema :: Schema}

instance FromJSON ToolSpec where
  parseJSON (Object v) =
    ToolSpec
      <$> (v .: "name")
      <*> (v .: "description")
      <*> (v .: "inputSchema")
  parseJSON _ = mempty

-- | Server reply to "tools/list".
data ToolListReply = ToolListReply {nextCursor :: Maybe Value, tools :: [ToolSpec]}

instance FromJSON ToolListReply where
  parseJSON (Object v) = ToolListReply <$> (v .:? "nextCursor") <*> (v .: "tools")
  parseJSON _ = mempty

newtype ToolCallReply = ToolCallReply T.Text

instance FromJSON ToolCallReply where
  parseJSON (Object v) = do
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
  parseJSON _ = mempty
