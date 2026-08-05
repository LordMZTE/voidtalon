module VoidTalon.TUI.Types
  ( Event (..),
    Name (..),
    RunState (..),
    isRunning,
    isStopped,
    warningA,
    barA,
    selectedA,
    toolTitleA,
    toolResultBorderA,
    toolPlanHeaderA,
    toolManagerToolTitleA,
    toolManagerSelectedA,
    toolManagerSchemaTypeA,
    toolManagerSchemaKeyA,
    bakedWidget,
    overlaySizeLimitPercent,
    PopupContext (..),
  )
where

import Brick (AttrName, Result, Size (Fixed), attrName)
import Brick.Types (Widget (Widget))
import Control.Concurrent (ThreadId)
import qualified Data.Text as T
import VoidTalon.Net.Completions (Update (UpdateMessage))
import VoidTalon.Util (SemiSemigroup ((<>?)))

data Event
  = EvCompletionUpdate Update
  | EvCompletionDone
  | EvClosePopup

instance SemiSemigroup Event where
  EvCompletionUpdate (UpdateMessage ma sa) <>? EvCompletionUpdate (UpdateMessage mb sb) =
    Just $ EvCompletionUpdate (UpdateMessage (ma <> mb) (sa <> sb))
  _ <>? _ = Nothing

data Name
  = NPromptField
  | NTimelineVP
  | NTimelineEntry Int
  | NToolDialog
  | NToolManager
  | NToolManagerVP
  | NToolManagerEntry Int
  | NHelp
  deriving (Eq, Ord, Show)

data RunState
  = RunStateStopped T.Text -- Stopped with reason
  | RunStateRunning ThreadId -- Running with given completions thread

isRunning :: RunState -> Bool
isRunning (RunStateStopped _) = False
isRunning (RunStateRunning _) = True

isStopped :: RunState -> Bool
isStopped = not . isRunning

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

-- | Attribute name used for the titles of tools in the tool manager
toolManagerToolTitleA :: AttrName
toolManagerToolTitleA = attrName "toolManagerToolTitle"

-- | Attribute name for a selected tool entry in the tool manager
toolManagerSelectedA :: AttrName
toolManagerSelectedA = attrName "toolManagerSelected"

-- | Attribute name used for the types shown in the tool manager's schema preview.
toolManagerSchemaTypeA :: AttrName
toolManagerSchemaTypeA = attrName "toolManagerSchemaType"

toolManagerSchemaKeyA :: AttrName
toolManagerSchemaKeyA = attrName "toolManagerSchemaKey"

-- | Utility to "un-render" a widget.  Useful if we need to know the size of the widget for some
-- surrounding context that the widget is then drawn into
bakedWidget :: Result a -> Widget a
bakedWidget = Widget Fixed Fixed . pure

-- | The limit of the available size in percent that ,should be used for overlays, if appropriate.
overlaySizeLimitPercent :: Int
overlaySizeLimitPercent = 75

data PopupContext = PopupContext
  { -- | When performed, the current popup is closed.
    close :: IO ()
  }
