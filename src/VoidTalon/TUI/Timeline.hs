module VoidTalon.TUI.Timeline (Entry (..), entryWidget) where

import Brick
import Brick.Widgets.Border
import qualified Data.Text as T
import qualified VoidTalon.TUI.Types as TT
import VoidTalon.Net.Completions (CompletionChoice(..))

data Entry
  = PromptEntry T.Text
  | OutputEntry CompletionChoice

messagePadding :: Padding
messagePadding = Pad 3

entryWidget :: Entry -> Widget TT.Name
entryWidget (PromptEntry p) = padLeft messagePadding $ border $ txtWrap p
entryWidget (OutputEntry (CompletionChoice reasoning reply)) =
  padRight messagePadding $ if T.null reasoning
    then replyWidget
    else reasoningWidget <=> replyWidget
  where
    reasoningWidget = borderWithLabel (str "Reasoning") $ txtWrap reasoning
    replyWidget = txtWrap reply
