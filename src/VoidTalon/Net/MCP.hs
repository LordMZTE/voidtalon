{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module VoidTalon.Net.MCP
  ( JSONRPCMessage (..),
    Transport (..),
    Connection (..),
    performInitialization,
  )
where

import Control.Concurrent (MVar, modifyMVar)
import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding
import qualified Data.ByteString.Lazy.Char8 as LBSIO
import qualified Data.Text as T
import PackageInfo_voidtalon (homepage, synopsis, version)
import System.IO (Handle)
import VoidTalon.Util (ToJSONEncoding (toEncoding))

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

-- | The message used to initialize a connection given a JSONRPC ID
initializationMessage :: Int -> JSONRPCMessage
initializationMessage id' =
  JSONRPCMessage
    { id = Just id',
      method = "initialize",
      params
    }
  where
    params =
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

-- | Perform initialization on an MCP connection
performInitialization :: Connection -> IO ()
performInitialization Connection {transport, nextId} = useId nextId $ \i -> do
  LBSIO.hPutStrLn transport.stdin $ encodingToLazyByteString $ toEncoding $ initializationMessage i
  -- TODO
  pure ()
