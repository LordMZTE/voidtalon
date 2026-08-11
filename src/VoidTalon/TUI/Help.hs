{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.TUI.Help (draw, handleEvent) where

import Brick
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import qualified Graphics.Vty as V
import VoidTalon.TUI.Types (Event (EvClosePopup), Name (NHelp), PopupContext (..))
import qualified VoidTalon.Util as Util

draw :: Widget Name
draw = withVScrollBars OnRight . viewport NHelp Vertical $ txtWrap helpText

vpScroll :: ViewportScroll Name
vpScroll = viewportScroll NHelp

handleEvent :: PopupContext -> BrickEvent Name e -> EventM Name s ()
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'k') [])) = vScrollBy vpScroll $ -1
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'j') [])) = vScrollBy vpScroll 1
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey V.KEsc [])) =
  liftIO $ Util.blockWriteBufferedBChan EvClosePopup evchan
handleEvent _ _ = pure ()

helpText :: T.Text
helpText =
  T.unlines
    [ "== General ==",
      "<C-q>        Quit",
      "<C-w>        Change focus between Timeline and Input",
      "<C-y/e>      Scroll up/down",
      "<C-c>        Cancel current operation",
      "<C-t>        Open Tool Manager",
      "<C-s>        Open Model Selector",
      "<F1>         Open this help window",
      "<Esc>        Close an open popup, such as this help window",
      "",
      "== Prompt Field ==",
      "<M-Cr>       Insert a literal newline",
      "<C-x>        Open external editor",
      "",
      "== Timeline ==",
      "k/j          Move selection up/down",
      "d            Delete selected item",
      "e            Edit selected item",
      "<CR>         Regenerate response",
      "",
      "== Tool Dialog ==",
      "y            Confirm",
      "n            Deny",
      "s            Spoof",
      "",
      "== Tool Manager ==",
      "k/j          Move selection up/down",
      "<Space>      Toggle selection",
      "",
      "== Model Selector ==",
      "k/j/g/G/...  Move selection",
      "<CR>         Select model",
      "r            Reload list"
    ]
