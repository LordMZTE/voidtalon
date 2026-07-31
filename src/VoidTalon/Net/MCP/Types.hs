{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Net.MCP.Types
  ( JSONRPCMessage (..),
    JSONRPCReply (..),
    ServerCapabilities (..),
    InitializeReply (..),
    RPCFailure (..),
    InitFailure (..),
    ToolSpec (..),
    ToolListReply (..),
  )
where

import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding
import Data.Aeson.KeyMap (member)
import qualified Data.Text as T
import VoidTalon.JSON (Schema, ToJSONEncoding (toEncoding))

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
      <*> (v .: "instructions")
  parseJSON _ = mempty

data RPCFailure
  = -- | Could not decode JSON from server
    RPCFailureDecode
  | -- | Server responded with bad message ID
    RPCFailureIDMismatch

data InitFailure
  = -- | Communication with server Failed
    InitFailureRPC RPCFailure
  | -- | Server does not have to tools capability
    InitFailureNoTools

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
  parseJSON (Object v) = ToolListReply <$> (v .: "nextCursor") <*> (v .: "tools")
  parseJSON _ = mempty
