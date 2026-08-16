{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Main (main) where

import Brick.Main (customMainWithDefaultVty)
import Control.Exception (bracket, throwIO, try)
import Control.Exception.Base (SomeException)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Graphics.Vty as Vty
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (shell)
import Toml.Schema (Result (Failure, Success))
import qualified VoidTalon.CLI as CLI
import qualified VoidTalon.Config as Config
import qualified VoidTalon.Log as Log
import qualified VoidTalon.Net.MCP as MCP
import qualified VoidTalon.TUI as TUI
import qualified VoidTalon.Tools as Tools
import qualified VoidTalon.Tools.ReadFile
import qualified VoidTalon.Tools.RunCommand
import qualified VoidTalon.Tools.WriteFile
import VoidTalon.Util (BufferedBChan (ch), newBufferedBChan)

main :: IO ()
main =
  bracket
    Log.init
    (const Log.deinit)
    (const mainWithLog)

mainWithLog :: IO ()
mainWithLog = do
  args <- CLI.readArguments
  config_content <- fromMaybe Config.findDefaultPath (pure <$> args.config) >>= readConfig
  config <- case Config.parseConfig config_content of
    Failure errs -> do
      hPutStrLn stderr "Failed to parse configuration:"
      mapM_ (hPutStrLn stderr) errs
      exitFailure
    Success warns conf -> do
      when (warns /= []) $ hPutStrLn stderr "Warnings while parsing config:"
      mapM_ (hPutStrLn stderr) warns
      pure conf
  httpMan <- HTTP.newManager HTTP.tlsManagerSettings
  mcps <- startStdioMCPServers args.mcp
  chan <- newBufferedBChan
  initState <-
    TUI.mkInitialState
      config
      chan
      httpMan
      config.model.standard
      (builtinTools ++ concatMap snd mcps)
  (_, vty) <- customMainWithDefaultVty (Just chan.ch) TUI.app initState
  Vty.shutdown vty
  sequence_ (MCP.closeConnection . fst <$> mcps)

readConfig :: FilePath -> IO Text
readConfig path = do
  config_result <- try $ Data.ByteString.readFile path
  case config_result :: Either SomeException ByteString of
    Left err ->
      ( hPutStrLn stderr $
          "Could not read config file!  Make sure to create it first!\n"
            <> (show err)
      )
        >> exitFailure
    Right conf -> pure $ decodeUtf8 conf

builtinTools :: [(T.Text, Tools.Tool)]
builtinTools =
  [ ("read_file", VoidTalon.Tools.ReadFile.tool),
    ("write_file", VoidTalon.Tools.WriteFile.tool),
    ("run_command", VoidTalon.Tools.RunCommand.tool)
  ]

startStdioMCPServers :: [String] -> IO [(MCP.Connection, [(T.Text, Tools.Tool)])]
startStdioMCPServers = sequence . fmap startOne
  where
    startOne cmd = do
      putStrLn $ "starting MCP server `" <> cmd <> "`"
      let spec = shell cmd
      mcp <- MCP.spawnStdio spec
      initRes <- MCP.performInitialization mcp
      case initRes of
        Left err -> throwIO err
        Right tools -> pure (mcp, tools)
