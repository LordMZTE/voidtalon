{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Net.MCP
  ( Transport (..),
    Connection (..),
    InitFailure (..),
    closeConnection,
    spawnStdio,
    performInitialization,
    module VoidTalon.Net.MCP.Types,
  )
where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Exception (throwIO)
import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as AKM
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (toList)
import qualified Data.Text as T
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
import VoidTalon.JSON (ParseEither (PLeft, PRight), ToJSONEncoding (toEncoding))
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
data Transport = TransportStdio {stdin :: Handle, stdout :: Handle, processHandle :: ProcessHandle}

data Connection = Connection {transport :: Transport, nextId :: MVar Int}

closeConnection :: Connection -> IO ()
closeConnection Connection {transport} = case transport of
  TransportStdio {processHandle} -> terminateProcess processHandle

-- | Spawn an MCP server with stdio transport.
-- std_in, std_out, and std_err fields of given @CreateProcess@ are overwritten.
spawnStdio :: CreateProcess -> IO Connection
spawnStdio spec = do
  let spec' = spec {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}
  (Just stdin, Just stdout, Nothing, processHandle) <- createProcess spec'
  nextId <- newMVar 0
  pure $ Connection {transport = TransportStdio {stdin, stdout, processHandle}, nextId}

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
        hFlush transport.stdin
    )
      >> pure i
  receiveReply callID
  where
    receiveReply callID = do
      reply <- BS8.hGetLine transport.stdout
      case eitherDecode $ LBS8.fromStrict reply of
        Left e -> pure $ Left $ RPCFailureDecode e
        Right (PLeft JSONRPCReply {id = id'}) | id' /= callID -> pure $ Left RPCFailureIDMismatch
        Right (PLeft JSONRPCReply {result}) -> pure $ Right result
        Right (PRight JSONRPCEvent {}) -> receiveReply callID

-- | Perform initialization on an MCP connection
performInitialization :: Connection -> IO (Either InitFailure [(T.Text, Tools.Tool)])
performInitialization con = do
  reply <- jsonRPCCall con methodInitialize initializationParams
  case reply of
    Left e -> pure . Left $ InitFailureRPC e
    Right
      ( InitializeReply
          { capabilities = ServerCapabilities {tools = False}
          }
        ) ->
        pure $ Left InitFailureNoTools
    Right _ -> do
      LBS8.hPutStrLn con.transport.stdin . encodingToLazyByteString . toEncoding
        $ JSONRPCMessage {params=emptyObject_, method=methodNotifInitialized, id=Nothing}
      bimap InitFailureRPC (liftA2 (,) (.name) (makeToolForSpec con) <$>)
        <$> listTools con Nothing

listTools :: Connection -> Maybe Value -> IO (Either RPCFailure [ToolSpec])
listTools con page = do
  let params = case page of
        Nothing -> emptyObject_
        Just p -> pairs $ "cursor" .= p
  reply <- jsonRPCCall con methodToolsList params
  case reply of
    Left e -> pure $ Left e
    Right ToolListReply {nextCursor, tools} -> case nextCursor of
      Just page' ->
        listTools con (Just page') >>= \case
          Left e -> pure $ Left e
          Right tools' -> pure . Right $ tools ++ tools'
      Nothing -> pure . Right $ tools

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
      reply <- jsonRPCCall con methodToolsCall params
      case reply of
        Left err -> throwIO err
        Right (ToolCallReply reply') -> pure reply'

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
