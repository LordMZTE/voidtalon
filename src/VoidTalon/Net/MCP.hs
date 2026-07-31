{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Net.MCP
  ( Transport (..),
    Connection (..),
    InitFailure (..),
    performInitialization,
    module VoidTalon.Net.MCP.Types,
  )
where

import Control.Concurrent (MVar, modifyMVar)
import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as T
import PackageInfo_voidtalon (homepage, synopsis, version)
import System.IO (Handle)
import VoidTalon.JSON (ToJSONEncoding (toEncoding))
import VoidTalon.Net.MCP.Types
import qualified VoidTalon.Tools as Tools

-- | The parameters passed to the "initialize" method.
initializationParams :: Encoding
initializationParams =
  pairs $
    mconcat
      [ -- Frustratingly, I'm writing this code two days after a new MCP version came out, but
        -- of course, that's useless at the moment.
        "protocolVersion" .= ("2025-11-25" :: T.Text),
        pair "capabilities" emptyObject_,
        pair "clientInfo" $
          pairs $
            mconcat
              [ "name" .= ("VoidTalon" :: T.Text),
                "title" .= ("VoidTalon" :: T.Text),
                "description" .= synopsis,
                "version" .= version,
                "websiteUrl" .= homepage
              ]
      ]

-- | A connection to an MCP server
-- TODO: HTTP transport
data Transport = TransportStdio {stdin :: Handle, stdout :: Handle}

data Connection = Connection {transport :: Transport, nextId :: MVar Int}

useId :: MVar Int -> (Int -> IO a) -> IO a
useId mv f = modifyMVar mv $ sequence . liftA2 (,) (+ 1) f

jsonRPCCall ::
  (FromJSON r) =>
  Connection ->
  -- | Name of the method to call
  T.Text ->
  -- | Params to the method
  Encoding ->
  IO (Either RPCFailure r)
jsonRPCCall Connection {transport, nextId} method params = do
  callID <- useId nextId $ \i ->
    ( do
        let msg = JSONRPCMessage {id = Just i, method, params}
        LBS8.hPutStrLn transport.stdin . encodingToLazyByteString $ toEncoding msg
    )
      >> pure i
  reply <- BS8.hGetLine transport.stdout
  pure $ case decode $ LBS8.fromStrict reply of
    Nothing -> Left RPCFailureDecode
    Just (JSONRPCReply {id = id'}) | id' /= callID -> Left RPCFailureIDMismatch
    Just (JSONRPCReply {result}) -> Right result

-- | Perform initialization on an MCP connection
performInitialization :: Connection -> IO (Either InitFailure [(T.Text, Tools.Tool)])
performInitialization con = do
  reply <- jsonRPCCall con "initialize" initializationParams
  case reply of
    Left e -> pure . Left $ InitFailureRPC e
    Right
      ( InitializeReply
          { capabilities = ServerCapabilities {tools = False}
          }
        ) ->
        pure $ Left InitFailureNoTools
    Right _ ->
      bimap InitFailureRPC (liftA2 (,) (.name) (makeToolForSpec con) <$>)
        <$> listTools con Nothing

listTools :: Connection -> Maybe Value -> IO (Either RPCFailure [ToolSpec])
listTools con page = do
  let params = case page of
        Nothing -> emptyObject_
        Just p -> pairs $ "cursor" .= p
  reply <- jsonRPCCall con "tools/list" params
  case reply of
    Left e -> pure $ Left e
    Right ToolListReply {nextCursor, tools} -> case nextCursor of
      Just page' ->
        listTools con (Just page') >>= \case
          Left e -> pure $ Left e
          Right tools' -> pure . Right $ tools ++ tools'
      Nothing -> pure . Right $ tools

makeToolForSpec :: Connection -> ToolSpec -> Tools.Tool
makeToolForSpec con ToolSpec {inputSchema, description} =
  Tools.Tool
    { description =
        Tools.Description
          { description,
            schema = inputSchema
          },
      invoke
    }
  where
    invoke input = do
      val <- eitherDecodeStrictText input
      pure ([], perform (val :: Value))
    perform val = undefined -- TODO
