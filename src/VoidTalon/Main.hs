{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Main (main) where

import Brick.Main (customMainWithDefaultVty)
import Control.Exception (bracket, fromException, try)
import Control.Exception.Base (SomeException)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Graphics.Vty as Vty
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Process (shell)
import Toml.Schema (Result (Failure, Success))
import qualified VoidTalon.CLI as CLI
import qualified VoidTalon.Config as Config
import qualified VoidTalon.Log as Log
import qualified VoidTalon.Net.MCP as MCP
import qualified VoidTalon.TUI as TUI
import qualified VoidTalon.Timeline as Timeline
import qualified VoidTalon.Util as Util

main :: IO ()
main =
  bracket
    Log.init
    (const Log.deinit)
    (const mainWithLog)

mainWithLog :: IO ()
mainWithLog = do
  args <- CLI.readArguments
  configDir <- maybe Config.findDefaultDir pure args.config
  configContent <- readConfig configDir
  config <- case Config.parseConfig configContent of
    Failure errs -> do
      hPutStrLn stderr "Failed to parse configuration:"
      mapM_ (hPutStrLn stderr) errs
      exitFailure
    Success warns conf -> do
      when (warns /= []) $ hPutStrLn stderr "Warnings while parsing config:"
      mapM_ (hPutStrLn stderr) warns
      pure conf
  httpMan <- HTTP.newManager HTTP.tlsManagerSettings
  mcps <-
    mconcat
      <$> sequence
        [ startStdioMCPServers args.mcp,
          startHTTPMCPServers httpMan args.mcpHttp
        ]
  chan <- Util.newBufferedBChan
  initEvents <- case args.prompt of
    CLI.PONone -> pure []
    CLI.POText t -> pure [TUI.EvAppendTimelineAndStart [Timeline.PromptEntry t]]
    CLI.POFile path -> do
      content <- TIO.readFile path
      pure [TUI.EvAppendTimelineAndStart [Timeline.PromptEntry content]]
  initState <-
    TUI.mkInitialState
      config
      configDir
      chan
      httpMan
      (snd <$> mcps)
  Util.blockWriteBufferedBChanAllRev chan initEvents
  (_, vty) <- customMainWithDefaultVty (Just chan.ch) TUI.app initState
  Vty.shutdown vty
  sequence_ (MCP.closeConnection . fst <$> mcps)

readConfig :: FilePath -> IO Text
readConfig dir = do
  let path = dir </> Config.fileName
  config_result <- try $ Data.ByteString.readFile path
  case config_result :: Either SomeException ByteString of
    Left err -> case fromException err of
      Just ioe | isDoesNotExistError ioe -> do
        hPutStrLn stderr "Config file doesn't exist.  Creating example config..."
        createDirectoryIfMissing True dir
        TIO.writeFile path Config.exampleConfig
        hPutStrLn stderr $ mconcat ["Created example config at ", path, ", please edit it."]
        exitFailure
      _ ->
        ( hPutStrLn stderr $
            "Could not read config file!\n"
              <> (show err)
        )
          >> exitFailure
    Right conf -> pure $ decodeUtf8 conf

startStdioMCPServers :: [String] -> IO [(MCP.Connection, MCP.Server)]
startStdioMCPServers = mapM startOne
  where
    startOne cmd = do
      putStrLn $ "starting MCP server `" <> cmd <> "`"
      let spec = shell cmd
      mcp <- MCP.spawnStdio spec
      caps <- MCP.performInitialization mcp
      pure (mcp, caps)

startHTTPMCPServers ::
  HTTP.Manager ->
  [MCP.HTTPConnectionSpec] ->
  IO [(MCP.Connection, MCP.Server)]
startHTTPMCPServers man = mapM startOne
  where
    startOne spec = do
      putStrLn $ "connecting to MCP server `" <> show spec.uri <> "`"
      mcp <- MCP.connectHTTP man spec
      caps <- MCP.performInitialization mcp
      pure (mcp, caps)
