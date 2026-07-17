{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI
  ( app,
    mkInitialState,
    State (..),
    module VoidTalon.TUI.Types,
  )
where

import Brick
import Brick.BChan
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import Data.Text.Zipper (breakLine, clearZipper)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH (makeLensesFor)
import qualified Network.HTTP.Client as HTTP
import VoidTalon.Config (Config (..), ConnectionConfig (..), TomlURI (..))
import VoidTalon.Net.Completions (Update (..))
import qualified VoidTalon.Net.Completions as Completions
import VoidTalon.Net.Models (ModelInfo (id))
import qualified VoidTalon.TUI.Timeline as Timeline
import VoidTalon.TUI.Types
import qualified VoidTalon.Timeline as Timeline

data State = State
  { config :: Config,
    evchan :: BChan Event,
    focus :: FocusRing Name,
    promptEditor :: Editor T.Text Name,
    httpMan :: HTTP.Manager,
    -- | These are stored in reverse since we update the latest message.
    timeline :: [Timeline.Entry],
    -- | The model to use.  This will be expanded to be a list later, so the user can select in the
    -- TUI
    model :: ModelInfo,
    -- | Nothing if running, Just reason_string if stopped.
    stopReason :: Maybe T.Text
  }

makeLensesFor
  [ ("promptEditor", "statePromptEditorL"),
    ("timeline", "stateTimelineL"),
    ("stopReason", "stateStopReasonL")
  ]
  ''State

mkInitialState :: Config -> BChan Event -> HTTP.Manager -> ModelInfo -> IO State
mkInitialState config evchan httpMan model =
  pure $
    State
      { config,
        evchan,
        httpMan,
        model,
        focus = focusRing [NPromptField],
        promptEditor = editorText NPromptField Nothing "",
        timeline = [],
        stopReason = Just "stop"
      }

type App' = App State Event Name

app :: App'
app =
  App
    { appDraw = draw,
      appChooseCursor = focusRingCursor (.focus),
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap =
        const $
          attrMap
            V.defAttr
            [ (warningA, fg V.yellow),
              (barA, V.magenta `on` (V.Color240 $ 235 - 16))
            ]
    }

outputVPScroll :: ViewportScroll Name
outputVPScroll = viewportScroll NOutputVP

draw :: State -> [Widget Name]
draw st = [vBox [output, hBorder, (joinBorders prompt), statusBar]]
  where
    output = Timeline.draw st.timeline
    prompt =
      withFocusRing
        st.focus
        (\b e -> vLimit 8 $ renderEditor (txt . T.unlines) b e)
        $ st.promptEditor
    statusBar = withAttr barA $ statusBarLeft <+> padLeft Max statusBarRight
    statusBarRight = txt $ fromMaybe "running" st.stopReason
    statusBarLeft = txt "<C-q> Quit | <C-e/y> Scl | <M-Cr> Ins. NL"

handleEvent :: BrickEvent Name Event -> EventM Name State ()
-- Exit with <C-q>
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
-- Scroll with <C-e> and <C-y>
handleEvent (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = vScrollBy outputVPScroll 1
handleEvent (VtyEvent (V.EvKey (V.KChar 'y') [V.MCtrl])) = vScrollBy outputVPScroll $ -1
-- TODO: <> on ByteString is slow (O(n)), optimize
handleEvent (AppEvent (EvCompletionUpdate (UpdateMessage added))) = do
  -- append text to output
  zoom stateTimelineL $ modify $ \case
    (Timeline.OutputEntry prev) : tl -> (Timeline.OutputEntry (prev <> added)) : tl
    tl -> (Timeline.OutputEntry added) : tl
  -- stick to bottom
  maybeVP <- lookupViewport NOutputVP
  case maybeVP of
    Just vp ->
      let top = vp ^. vpTop
          (_, vpHeight) = vp ^. vpSize
          (_, contentHeight) = vp ^. vpContentSize
          visCols = contentHeight - top
          isAtBottom = visCols <= vpHeight
       in when isAtBottom $ vScrollToEnd outputVPScroll
    Nothing -> pure ()
handleEvent (AppEvent (EvCompletionUpdate (UpdateStop reason))) =
  zoom stateStopReasonL $ put $ Just reason
handleEvent ev = do
  st <- get
  case focusGetCurrent $ st.focus of
    Just NPromptField -> case ev of
      -- I would prefer if this were shift+enter rather than meta+enter, but that doesn't seem to be
      -- supported by vty.
      (VtyEvent (V.EvKey V.KEnter [V.MMeta])) ->
        zoom statePromptEditorL $ modify $ applyEdit breakLine
      (VtyEvent (V.EvKey V.KEnter [])) -> do
        let running = isNothing st.stopReason
        let prompt = mconcat $ getEditContents $ st.promptEditor
        unless (running || T.null prompt) $ do
          -- clear entry
          zoom statePromptEditorL $ modify $ applyEdit clearZipper

          -- append prompt to timeline
          zoom stateTimelineL $ modify (Timeline.PromptEntry prompt :)

          -- set runStatus to running
          zoom stateStopReasonL $ put Nothing

          -- start completions request
          ctx <- zoom stateTimelineL $ (Completions.Context st.model.id . reverse) <$> get
          liftIO $
            Completions.perform
              ( writeBChan (st.evchan)
                  . EvCompletionUpdate
              )
              (st.config).connection.base_url.inner
              (st.httpMan)
              ctx
      _ -> zoom statePromptEditorL $ handleEditorEvent ev
    _ -> pure ()
