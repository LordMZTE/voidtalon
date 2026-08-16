module VoidTalon.Log
  ( init,
    deinit,
    logWith,
    logStr,
    logTxt,
    debug,
    info,
    warn,
    err,
    debugT,
    infoT,
    warnT,
    errT,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, withMVar)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (AppendMode), hClose, hFlush, hPutStrLn, openFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (getCurrentPid)
import Prelude hiding (init)

{-# NOINLINE logFileHandle #-}
logFileHandle :: MVar Handle
logFileHandle = unsafePerformIO newEmptyMVar

init :: IO ()
init = do
  tmpdir <- getTemporaryDirectory
  pid <- getCurrentPid
  let logpath = tmpdir </> (mconcat ["voidtalon-", show pid, ".log"])
  file <- openFile logpath AppendMode
  putMVar logFileHandle file

deinit :: IO ()
deinit = takeMVar logFileHandle >>= hClose

logWith :: (IsString a, Semigroup a) => (Handle -> a -> IO ()) -> String -> a -> IO ()
logWith printer scope msg = withMVar logFileHandle go
  where
    go h = do
      printer h $ fromString prefix <> msg
      hFlush h
    prefix = mconcat ["[", scope, "] "]

logStr :: String -> String -> IO ()
logStr = logWith hPutStrLn

logTxt :: String -> T.Text -> IO ()
logTxt = logWith T.hPutStrLn

debug :: String -> IO ()
debug = logStr "debug"

info :: String -> IO ()
info = logStr "info"

warn :: String -> IO ()
warn = logStr "warn"

err :: String -> IO ()
err = logStr "err"

debugT :: T.Text -> IO ()
debugT = logTxt "debug"

infoT :: T.Text -> IO ()
infoT = logTxt "info"

warnT :: T.Text -> IO ()
warnT = logTxt "warn"

errT :: T.Text -> IO ()
errT = logTxt "err"
