module VoidTalon.TUI.Types
  ( Event (..),
    Name (..),
    warningA,
    barA,
    selectedA,
    bakedWidget,
  )
where

import Brick (AttrName, Result, Size (Fixed), attrName)
import Brick.Types (Widget (Widget))
import VoidTalon.Net.Completions (Update)

data Event = EvCompletionUpdate Update

data Name
  = NPromptField
  | NTimelineVP
  | NTimelineEntry Int
  deriving (Eq, Ord, Show)

-- | Attributes for warning text
warningA :: AttrName
warningA = attrName "warning"

-- | Attribute name for the status bar
barA :: AttrName
barA = attrName "bar"

-- | Attribute name for selected timeline entries
selectedA :: AttrName
selectedA = attrName "selected"

-- | Utility to "un-render" a widget.  Useful if we need to know the size of the widget for some
-- surrounding context that the widget is then drawn into
bakedWidget :: Result a -> Widget a
bakedWidget = Widget Fixed Fixed . pure
