{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Util
  ( untab,
    editInEditor,
    remove,
    SemiSemigroup (..),
    BufferedBChan (..),
    newBufferedBChan,
    pushBufferedBChan,
    writeBufferedBChan,
    flushBufferedBChan,
    blockWriteBufferedBChan,
  )
where

import Brick.BChan (BChan, newBChan, writeBChan, writeBChanNonBlocking)
import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Control.Exception (finally)
import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Process (callProcess)
import System.Random.Stateful (globalStdGen, randomRM)

-- | Replaces all tabs with four spaces.  We do this because feeding tabs to VTY causes
-- breakage, and it's cheapest to do here where all strings are still short.
--
-- Note that in rare cases, this breaks syntax highlighting with formats that are picky with
-- spaces and tabs.  Try to make the AI generate a makefile, and you'll get what I mean.
untab :: T.Text -> T.Text
untab = T.replace "\t" "    "

-- | Given a file extension and some Text, invokes the user's editor to edit that text and returns
-- the updated content.
editInEditor :: String -> LT.Text -> IO LT.Text
editInEditor ext t = do
  editor <-
    fromMaybe "ed" -- fall back to the standard unix editor
      <$> lookupEnv "EDITOR"
  tmpdir <- getTemporaryDirectory
  rand <- replicateM 8 $ randomRM ('a', 'z') globalStdGen
  let path = tmpdir </> ("voidtalon-" <> rand <> "." <> ext)
  LT.writeFile path t
  res <- finally (callProcess editor [path] >> LT.readFile path) (removeFile path)
  pure res

-- | Removes the nth element from a list keeping all others.
-- no-op if the number is out-of-bounds.
remove :: (Num n, Eq n) => n -> [a] -> [a]
remove _ xs@[] = xs
remove 0 (_ : xs) = xs
remove n (x : xs) = x : (remove (n - 1) xs)

-- | Like a Semigroup, but concatenation may not be possible in all cases
class SemiSemigroup m where
  -- | Must uphold the following laws:
  -- - (a <>? b) >>= (<>? c) == (a <>?) =<< (b <>? c) (associativity)
  (<>?) :: m -> m -> Maybe m

-- | A wrapper around a BChan that can chunk events.  This is needed because Brick redraws the TUI
-- after every event.  This makes no sense and needlessly slows down our application.  This is
-- essentially used to implement the behavior that all pending events are consumed before we redraw
-- the TUI again.
data BufferedBChan e = BufferedBChan {ch :: BChan [e], buf :: MVar [e]}

newBufferedBChan :: IO (BufferedBChan e)
newBufferedBChan = do
  chan <- newBChan 1
  buf <- newMVar []
  pure $ BufferedBChan chan buf

-- | Pushes a message to the BChan's buffer.
pushBufferedBChan :: (SemiSemigroup e) => e -> [e] -> [e]
pushBufferedBChan e [] = [e]
pushBufferedBChan e (x : xs) = case x <>? e of
  Just e' -> e' : xs
  Nothing -> e : x : xs

-- | Attempt to send a message to the buffered channel, semisemigroupily buffering it if the channel
-- is full.
writeBufferedBChan :: (SemiSemigroup e) => e -> BufferedBChan e -> IO ()
writeBufferedBChan ev (BufferedBChan ch mbuf) = modifyMVar_ mbuf sendWithBuffer
  where
    sendWithBuffer buf = do
      let buf' = pushBufferedBChan ev buf
      success <- writeBChanNonBlocking ch buf'
      pure $ if success then [] else buf'

-- | Flushes a BufferedBChan, blocking util everything has been written.
flushBufferedBChan :: BufferedBChan e -> IO ()
flushBufferedBChan (BufferedBChan ch mbuf) = modifyMVar_ mbuf $ (>> pure []) . writeBChan ch

-- | Like calling writeBufferedBChan and then flushing, but more efficient.
blockWriteBufferedBChan :: (SemiSemigroup e) => e -> BufferedBChan e -> IO ()
blockWriteBufferedBChan ev (BufferedBChan ch mbuf) =
  modifyMVar_ mbuf $ (>> pure []) . writeBChan ch . pushBufferedBChan ev
