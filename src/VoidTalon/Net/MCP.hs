{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VoidTalon.Net.MCP
  ( Transport (..),
    Connection (..),
    closeConnection,
    spawnStdio,
    connectHTTP,
    Server (..),
    performInitialization,
    module VoidTalon.Net.MCP.Types,
  )
where

import Control.Arrow ((&&&))
import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Exception (throwIO)
import Control.Monad.State.Strict (StateT, execStateT, liftIO, put)
import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (find, toList)
import qualified Data.Text as T
import Network.HTTP.Client (httpNoBody)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import PackageInfo_voidtalon (homepage, synopsis, version)
import System.IO (Handle, hFlush)
import System.Process
  ( CreateProcess (std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe, Inherit),
    createProcess,
    std_err,
    terminateProcess,
  )
import VoidTalon.JSON (ToJSONEncoding (toEncoding))
import qualified VoidTalon.Log as Log
import VoidTalon.Net (checkStatusOK)
import VoidTalon.Net.MCP.Types
import qualified VoidTalon.Net.SSE as SSE
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
data Transport
  = TransportStdio {stdin :: Handle, stdout :: Handle, processHandle :: ProcessHandle}
  | TransportHTTP {man :: HTTP.Manager, baseReq :: HTTP.Request}

data Connection = Connection {transport :: Transport, nextId :: MVar Int}

closeConnection :: Connection -> IO ()
closeConnection Connection {transport} = case transport of
  TransportStdio {processHandle} -> terminateProcess processHandle
  TransportHTTP {} -> pure ()

-- | Spawn an MCP server with stdio transport.
-- std_in, std_out, and std_err fields of given @CreateProcess@ are overwritten.
spawnStdio :: CreateProcess -> IO Connection
spawnStdio spec = do
  Log.info $ "Starting stdio MCP server with spec: " <> show spec
  let spec' = spec {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}
  (Just stdin, Just stdout, Nothing, processHandle) <- createProcess spec'
  nextId <- newMVar 0
  pure $ Connection {transport = TransportStdio {stdin, stdout, processHandle}, nextId}

connectHTTP :: HTTP.Manager -> HTTPConnectionSpec -> IO Connection
connectHTTP man HTTPConnectionSpec {uri, headers} = do
  -- The MCP spec mandates this Accept header
  let headers' =
        (HTTP.hContentType, "application/json")
          : (HTTP.hAccept, "application/json,text/event-stream")
          : headers
  nextId <- newMVar 0
  baseReq' <- HTTP.requestFromURI uri
  let baseReq = baseReq' {HTTP.method = "POST", HTTP.requestHeaders = headers'}
  pure $ Connection {transport = TransportHTTP {man, baseReq}, nextId}

useId :: MVar Int -> IO Int
useId mv = modifyMVar mv $ pure . ((+ 1) &&& id)

jsonRPCCall ::
  forall r.
  (FromJSON r) =>
  Connection ->
  -- | Name of the method to call
  T.Text ->
  -- | Params to the method
  Encoding ->
  IO r
jsonRPCCall Connection {transport, nextId} method params = case transport of
  TransportStdio {stdin, stdout} -> do
    callID <- useId nextId
    let msg = JSONRPCMessage {id = Just callID, method, params}
    LBS8.hPutStrLn stdin . encodingToLazyByteString $ toEncoding msg
    hFlush stdin
    receiveReplyStdio callID stdout
  TransportHTTP {man, baseReq} -> do
    callID <- useId nextId
    let msg = JSONRPCMessage {id = Just callID, method, params}
    let req =
          baseReq
            { HTTP.requestBody = HTTP.RequestBodyLBS . encodingToLazyByteString $ toEncoding msg
            }
    HTTP.withResponse req man $ \res -> do
      checkStatusOK res
      let contentType = find ((HTTP.hContentType ==) . fst) res.responseHeaders
      case contentType of
        Just (_, "application/json") -> do
          contentChunks <- HTTP.brConsume res.responseBody
          let content = LBS.fromChunks contentChunks
          res' <- decodeResponse content
          case res' of
            JSONRPCServerReply JSONRPCReply {id = id'}
              | id' /= callID -> throwIO RPCFailureIDMismatch
            JSONRPCServerReply JSONRPCReply {result} -> pure result
            JSONRPCServerEvent JSONRPCEvent {} -> throwIO RPCFailureNoResponse
        Just (_, "text/event-stream") -> do
          result <-
            execStateT
              (SSE.readStream res.responseBody $ receiveReplyHTTPSSE callID)
              Nothing
          case result of
            Just r -> pure r
            Nothing -> throwIO RPCFailureNoResponse
        Just (_, x) -> fail $ "Unexpected content type from MCP server: " <> show x
        Nothing -> throwIO RPCFailureNoResponse
  where
    receiveReplyStdio callID stdout = do
      reply <- BS8.hGetLine stdout
      res <- decodeResponse $ LBS8.fromStrict reply
      case res of
        JSONRPCServerReply JSONRPCReply {id = id'} | id' /= callID -> throwIO RPCFailureIDMismatch
        JSONRPCServerReply JSONRPCReply {result} -> pure result
        JSONRPCServerEvent JSONRPCEvent {} -> receiveReplyStdio callID stdout
    receiveReplyHTTPSSE :: Int -> SSE.Event -> StateT (Maybe r) IO ()
    receiveReplyHTTPSSE _ SSE.Event {content = ""} = pure ()
    receiveReplyHTTPSSE callID SSE.Event {content} = do
      res <- liftIO $ decodeResponse content
      case res of
        JSONRPCServerReply JSONRPCReply {id = id'}
          | id' /= callID -> liftIO $ throwIO RPCFailureIDMismatch
        JSONRPCServerReply JSONRPCReply {result} -> put $ Just result
        JSONRPCServerEvent JSONRPCEvent {} -> pure ()
    decodeResponse content = case eitherDecode content of
      Left e -> throwIO $ RPCFailureDecode e
      Right x -> pure x

jsonRPCNotify :: Connection -> T.Text -> Encoding -> IO ()
jsonRPCNotify con method params = case con.transport of
  TransportStdio {stdin} -> LBS8.hPutStrLn stdin . encodingToLazyByteString $ toEncoding msg
  TransportHTTP {man, baseReq} -> do
    let req =
          baseReq
            { HTTP.requestBody = HTTP.RequestBodyLBS . encodingToLazyByteString $ toEncoding msg
            }
    res <- httpNoBody req man
    checkStatusOK res
  where
    msg = JSONRPCMessage {params, method, id = Nothing}

data Server = Server
  { tools :: [(T.Text, Tools.Tool)],
    instructions :: Maybe T.Text,
    serverInfo :: ServerInfo
  }

-- | Perform initialization on an MCP connection
performInitialization :: Connection -> IO Server
performInitialization con = do
  reply <- jsonRPCCall con methodInitialize initializationParams
  case reply of
    InitializeReply
      { capabilities = ServerCapabilities {tools = False}
      } ->
        throwIO InitFailureNoTools
    InitializeReply {instructions, serverInfo} -> do
      jsonRPCNotify con methodNotifInitialized emptyObject_
      listToolsRes <- listTools con Nothing
      let tools = liftA2 (,) (.name) (makeToolForSpec con) <$> listToolsRes
      Log.info $ mconcat ["MCP server initialized with ", show $ length tools, " tools"]
      pure Server {tools, instructions, serverInfo}

listTools :: Connection -> Maybe Value -> IO [ToolSpec]
listTools con page = do
  let params = case page of
        Nothing -> emptyObject_
        Just p -> pairs $ "cursor" .= p
  ToolListReply {nextCursor, tools} <- jsonRPCCall con methodToolsList params
  case nextCursor of
    Just page' -> (tools ++) <$> listTools con (Just page')
    Nothing -> pure tools

makeToolForSpec :: Connection -> ToolSpec -> Tools.Tool
makeToolForSpec con ToolSpec {name, inputSchema, description} =
  Tools.Tool
    { description =
        Tools.Description
          { description,
            schema = Right inputSchema
          },
      invoke
    }
  where
    invoke input = do
      val <- eitherDecodeStrictText input
      pure (jsonPlan T.empty val, perform val)
    perform val = do
      let params =
            pairs $
              mconcat
                [ "name" .= name,
                  "arguments" .= val
                ]
      ToolCallReply repl <- jsonRPCCall con methodToolsCall params
      pure repl

jsonPlan :: T.Text -> Value -> Tools.Plan
jsonPlan p (Object o) = concatMap elemPlan $ AKM.toList o
  where
    elemPlan (k, v) = let p' = mconcat [p, ".", toText k] in jsonPlan p' v
jsonPlan p (Array a) = concatMap elemPlan $ zip [0 ..] (toList a)
  where
    elemPlan (k, v) = let p' = mconcat [p, "[", T.show (k :: Int), "]"] in jsonPlan p' v
jsonPlan p (String txt) = [(p, txt)]
jsonPlan p (Number n) = [(p, T.show n)]
jsonPlan p (Bool b) = [(p, T.show b)]
jsonPlan p Null = [(p, "null")]
