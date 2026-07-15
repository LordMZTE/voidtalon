module VoidTalon.TUI.Types
  ( Event (..),
    Name (..),
    warningA,
    bakedWidget,
  )
where

import Brick (AttrName, Result, Size (Fixed), attrName)
import Brick.Types (Widget (Widget))
import VoidTalon.Timeline (LLMMessage)

data Event = EvGetCompletions LLMMessage

data Name = NPromptField | NOutputVP deriving (Eq, Ord, Show)

-- | Attributes for warning text
warningA :: AttrName
warningA = attrName "warning"

-- | Utility to "un-render" a widget.  Useful if we need to know the size of the widget for some
-- surrounding context that the widget is then drawn into
bakedWidget :: Result a -> Widget a
bakedWidget = Widget Fixed Fixed . pure
