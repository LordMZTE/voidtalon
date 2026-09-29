{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module VoidTalon.Net.SSE (Event (..), nullEvent, readStream) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (ord)
import Data.Word (Word8)
import Lens.Micro
import Network.HTTP.Client (BodyReader)

data Event = Event
  { content :: LBS.ByteString,
    id :: Maybe LBS.ByteString
  }
  deriving (Eq)

nullEvent :: Event
nullEvent = Event {content = LBS.empty, id = Nothing}

-- | Reads events from an HTTP SSE stream and for each `data`-event, runs the given handler.
readStream :: forall m. (MonadIO m) => BodyReader -> (Event -> m ()) -> m ()
readStream reader handler = takeLines (mempty, nullEvent)
  where
    takeLines :: (BSB.Builder, Event) -> m ()
    takeLines state@(_, ev) =
      liftIO reader >>= \case
        c | BS.null c -> unless (ev == nullEvent) $ handler ev
        chunk -> do
          state' <- BS.foldl' foldChar (pure state) chunk
          takeLines state'

    foldChar :: m (BSB.Builder, Event) -> Word8 -> m (BSB.Builder, Event)
    foldChar prev ch
      -- Hit newline, consume buffer and start with new empty buffer
      | ch == (fromIntegral $ ord '\n') = do
          (line, ev) <- prev
          ev' <- processLine (BSB.toLazyByteString line) ev
          pure (mempty, ev')
      -- Not newline, append to buffer
      | otherwise = prev & mapped . _1 %~ (<> BSB.word8 ch)

    processLine :: LBS.ByteString -> Event -> m Event
    processLine l ev | l LBS.!? 0 == Just ':' = pure ev -- comment
    processLine (LBS.stripPrefix "data: " -> Just l) ev = pure ev {content = l}
    processLine (LBS.stripPrefix "id: " -> Just l) ev = pure ev {id = Just l}
    processLine l ev | LBS.null l = handler ev >> pure nullEvent -- empty line, consume event
    processLine _ ev = pure ev -- garbage
