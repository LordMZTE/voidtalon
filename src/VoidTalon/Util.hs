{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Util (untab, editInEditor, remove) where

import Control.Exception (finally)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Process (callProcess)
import System.Random.Stateful (globalStdGen, randomRM)
import Control.Monad (replicateM)

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
remove 0 (_:xs) = xs
remove n (x:xs) = x:(remove (n - 1) xs)
