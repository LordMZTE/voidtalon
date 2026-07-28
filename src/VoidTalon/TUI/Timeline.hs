{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI.Timeline
  ( State (..),
    initialState,
    stateFocusL,
    stateEntriesL,
    entryWidget,
    draw,
    editEntry,
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Function (applyWhen)
import qualified Data.IntMap as IntMap
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Lens.Micro.TH
import Skylighting as SL
import VoidTalon.TUI.Markdown (highlightedCode, markdownWidget)
import VoidTalon.TUI.Types (Name (..), selectedA, toolResultBorderA, toolTitleA)
import qualified VoidTalon.TUI.Types as TT
import qualified VoidTalon.Timeline as TL
import qualified VoidTalon.Tools as Tools
import VoidTalon.Util (editInEditor)

data State = State
  { -- | Index of the element in the timeline that's focused.  Starts from the bottom.
    focus :: Int,
    -- | These are stored in reverse since we update the latest message.
    entries :: [TL.Entry]
  }

makeLensesFor
  [ ("focus", "stateFocusL"),
    ("entries", "stateEntriesL")
  ]
  ''State

initialState :: State
initialState = State {focus = 0, entries = []}

messagePadding :: Padding
messagePadding = Pad 3

messageWidget :: Widget n -> Widget n
-- This double padding is to align the widget to the right and provide spacing on the left.
messageWidget = padLeft Max . padLeft messagePadding . border

entryWidget :: Bool -> TL.Entry -> Widget TT.Name
entryWidget sel (TL.PromptEntry p) =
  applyWhen sel (withAttr selectedA) inner
  where
    inner = messageWidget $ markdownWidget "prompt" p
entryWidget sel (TL.OutputEntry (TL.LLMMessage reasoning reply toolCalls)) =
  applyWhen sel (withAttr selectedA) inner
  where
    inner = padRight messagePadding $ vBox widgets
    widgets =
      (if T.null reasoning then [] else [reasoningWidget])
        ++ [replyWidget]
        ++ (toolsWidget <$> IntMap.elems toolCalls)

    reasoningWidget = borderWithLabel (txt "Reasoning") $ markdownWidget "reasoning" $ reasoning
    replyWidget = markdownWidget "reply" reply

    toolsWidget Tools.Call {id = _, name, parameters} =
      hCenter $
        borderWithLabel (withAttr toolTitleA $ txt name) $
          highlightedCode jsonSyntax parameters

    jsonSyntax = SL.defaultSyntaxMap Map.! "JSON"
entryWidget sel (TL.ToolResultEntry {id = _, content}) =
  applyWhen sel (withAttr selectedA) inner
  where
    inner = overrideAttr borderAttr toolResultBorderA $ messageWidget $ txtWrap content

draw ::
  -- | True iff the timeline is currently focused
  Bool ->
  State ->
  Widget TT.Name
draw focused State {focus, entries} =
  withVScrollBars OnRight
    . viewport NTimelineVP Vertical
    . vBox
    . fst
    $ foldl
      ( \(l, n) e ->
          ( (reportExtent (NTimelineEntry n) $ entryWidget (focused && n == focus) e) : l,
            n + 1
          )
      )
      ([], 0)
      entries

-- | Invoke the user's editor on the given entry
editEntry :: TL.Entry -> IO TL.Entry
editEntry (TL.PromptEntry t) =
  TL.PromptEntry . LT.toStrict <$> (editInEditor "md" $ LT.fromStrict t)
editEntry (TL.OutputEntry (TL.LLMMessage reasoning content toolCalls)) = do
  let separator = "\n" <> T.replicate 100 "=" <> "\n"
  let toEdit = LT.fromChunks [reasoning, separator, content]
  edited <- editInEditor "md" toEdit
  pure . TL.OutputEntry $ case LT.splitOn (LT.fromStrict separator) edited of
    [] -> mempty
    [content'] -> TL.LLMMessage T.empty (LT.toStrict content') toolCalls
    reasoning' : rest ->
      TL.LLMMessage
        (LT.toStrict reasoning')
        (LT.toStrict $ mconcat rest)
        toolCalls
editEntry (TL.ToolResultEntry {id = id', content}) = do
  edited <- editInEditor "md" $ LT.fromStrict content
  -- Can't use update syntax here because that gets us some weird compiler warning
  pure $ TL.ToolResultEntry {id = id', TL.content = LT.toStrict edited}
