module VoidTalon.Main (main) where

import Brick.BChan (newBChan)
import Brick.Main (customMainWithDefaultVty)
import Control.Exception (try)
import Control.Exception.Base (SomeException)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Graphics.Vty as Vty
import qualified Network.HTTP.Client as HTTP
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Toml.Schema (Result (Failure, Success))
import qualified VoidTalon.CLI as CLI
import qualified VoidTalon.Config as Config
import qualified VoidTalon.Net.Models as Models
import qualified VoidTalon.TUI as TUI

main :: IO ()
main = do
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
  httpMan <- HTTP.newManager HTTP.defaultManagerSettings
  models <- Models.list config.connection.base_url.inner httpMan
  model <- case models of
    m : _ -> pure m
    _ -> fail "The server reported an empty list of models!"
  -- We leave space for this many events because we might get incoming tokens while the user has
  -- the editor open.  In order to not block the network connection, we need to buffer those events.
  chan <- newBChan 1024
  initState <- TUI.mkInitialState config chan httpMan model
  (_, vty) <- customMainWithDefaultVty (Just chan) TUI.app initState
  Vty.shutdown vty

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
