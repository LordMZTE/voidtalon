module VoidTalon.TUI.Types
  ( Event (..),
    Name (..),
    warningA,
    barA,
    selectedA,
    toolTitleA,
    toolResultBorderA,
    toolPlanHeaderA,
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
  | NToolDialog
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

-- | Attribute name used for tool call titles
toolTitleA :: AttrName
toolTitleA = attrName "toolTitle"

-- | Attribute name used for the border around tool response timeline entries.
toolResultBorderA :: AttrName
toolResultBorderA = attrName "toolResultBorder"

-- | Attribute name used for the headers above plan entries in tħe tool confirmation dialog.
toolPlanHeaderA :: AttrName
toolPlanHeaderA = attrName "toolPlanHeader"

-- | Utility to "un-render" a widget.  Useful if we need to know the size of the widget for some
-- surrounding context that the widget is then drawn into
bakedWidget :: Result a -> Widget a
bakedWidget = Widget Fixed Fixed . pure
