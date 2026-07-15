{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.TUI.Timeline (entryWidget, draw) where

import Brick
import Brick.Widgets.Border
import qualified Data.Text as T
import VoidTalon.TUI.Markdown (markdownWidget)
import VoidTalon.TUI.Types (Name (..))
import qualified VoidTalon.TUI.Types as TT
import qualified VoidTalon.Timeline as TL

messagePadding :: Padding
messagePadding = Pad 3

entryWidget :: TL.Entry -> Widget TT.Name
entryWidget (TL.PromptEntry p) =
  -- This double padding is to align the widget to the right and provide spacing on the left.
  padLeft Max
    . padLeft messagePadding
    . border
    $ markdownWidget "prompt" p
entryWidget (TL.OutputEntry (TL.LLMMessage reasoning reply)) =
  padRight messagePadding $
    if T.null reasoning
      then replyWidget
      else reasoningWidget <=> replyWidget
  where
    reasoningWidget = borderWithLabel (txt "Reasoning") $ markdownWidget "reasoning" $ reasoning
    replyWidget = markdownWidget "reply" reply

draw :: [TL.Entry] -> Widget TT.Name
draw = withVScrollBars OnRight . viewport NOutputVP Vertical . vBox . foldl (flip $ (:) . entryWidget) []
