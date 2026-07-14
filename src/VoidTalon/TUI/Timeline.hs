{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.TUI.Timeline (entryWidget, draw) where

import Brick
import Brick.Widgets.Border
import qualified Data.Text as T
import VoidTalon.TUI.Types (Name (..))
import qualified VoidTalon.TUI.Types as TT
import qualified VoidTalon.Timeline as TL

messagePadding :: Padding
messagePadding = Pad 3

entryWidget :: TL.Entry -> Widget TT.Name
entryWidget (TL.PromptEntry p) = padLeft messagePadding $ border $ txtWrap p
entryWidget (TL.OutputEntry (TL.LLMMessage reasoning reply)) =
  padRight messagePadding $
    if T.null reasoning
      then replyWidget
      else reasoningWidget <=> replyWidget
  where
    reasoningWidget = borderWithLabel (txt "Reasoning") $ txtWrap reasoning
    replyWidget = txtWrap reply

draw :: [TL.Entry] -> Widget TT.Name
draw = withVScrollBars OnRight . viewport NOutputVP Vertical . vBox . foldl (flip $ (:) . entryWidget) []
