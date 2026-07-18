{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.TUI.Timeline (entryWidget, draw) where

import Brick
import Brick.Widgets.Border
import Data.Function (applyWhen)
import qualified Data.Text as T
import VoidTalon.TUI.Markdown (markdownWidget)
import VoidTalon.TUI.Types (Name (..), selectedA)
import qualified VoidTalon.TUI.Types as TT
import qualified VoidTalon.Timeline as TL

messagePadding :: Padding
messagePadding = Pad 3

entryWidget :: Bool -> TL.Entry -> Widget TT.Name
entryWidget sel (TL.PromptEntry p) =
  applyWhen sel (withAttr selectedA) inner
  where
    -- This double padding is to align the widget to the right and provide spacing on the left.
    inner =
      padLeft Max
        . padLeft messagePadding
        . border
        $ markdownWidget "prompt" p
entryWidget sel (TL.OutputEntry (TL.LLMMessage reasoning reply)) =
  applyWhen sel (withAttr selectedA) inner
  where
    inner =
      padRight messagePadding $
        if T.null reasoning
          then replyWidget
          else reasoningWidget <=> replyWidget
    reasoningWidget = borderWithLabel (txt "Reasoning") $ markdownWidget "reasoning" $ reasoning
    replyWidget = markdownWidget "reply" reply

draw :: Maybe Int -> [TL.Entry] -> Widget TT.Name
draw focus =
  withVScrollBars OnRight
    . viewport NTimelineVP Vertical
    . vBox
    . fst
    . foldl
      ( \(l, n) e ->
          ( (reportExtent (NTimelineEntry n) $ entryWidget (isFocused n) e) : l,
            n + 1
          )
      )
      ([], 0)
  where
    isFocused n = case focus of
      Nothing -> False
      Just m -> n == m
