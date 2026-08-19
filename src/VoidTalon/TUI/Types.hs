module VoidTalon.TUI.Types
  ( Event (..),
    Name (..),
    RunState (..),
    isRunning,
    isStopped,
    warningA,
    errorA,
    barA,
    selectedA,
    toolTitleA,
    toolResultBorderA,
    toolPlanHeaderA,
    toolManagerToolTitleA,
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
import qualified Data.Vector as Vec
import qualified Network.HTTP.Client as HTTP
import VoidTalon.Config (Config)
import VoidTalon.Net.Completions (Update (UpdateMessage))
import VoidTalon.Net.Models (ModelInfo)
import VoidTalon.Util (BufferedBChan, SemiSemigroup ((<>?)))

-- | Our Brick event type for events sent to the TUI
-- Remeber to check if the SemiSemigroup instance can be expanded when adding something here!
data Event
  = -- | Some new completion data came in, append to timeline
    EvCompletionUpdate Update
  | -- | Completions have finishes, reset run state
    EvCompletionDone
  | -- | The currently open popup has signeled to be closed
    EvClosePopup
  | -- | The server answered a query to the models endpoint
    EvModelList (Vec.Vector ModelInfo)
  | -- | An error has happened, display error popup.
    EvError String

instance SemiSemigroup Event where
  EvCompletionUpdate (UpdateMessage ma sa) <>? EvCompletionUpdate (UpdateMessage mb sb) =
    Just $ EvCompletionUpdate (UpdateMessage (ma <> mb) (sa <> sb))
  EvError _ <>? EvError e = Just $ EvError e -- we always consider the latest error
  _ <>? _ = Nothing

data Name
  = NPromptField
  | NErrorPopup
  | NHelp
  | NModelSelector
  | NTimelineEntry Int
  | NTimelineVP
  | NToolDialog
  | NToolManager
  | NToolManagerEntry Int
  | NToolManagerVP
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

-- | Attributes for error text
errorA :: AttrName
errorA = attrName "error"

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
  { config :: Config,
    httpMan :: HTTP.Manager,
    evchan :: BufferedBChan Event
  }
