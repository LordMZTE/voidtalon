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
import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Zipper (breakLine, clearZipper, textZipper)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLensesFor)
import qualified Network.HTTP.Client as HTTP
import VoidTalon.Config (Config (..), ConnectionConfig (..), TomlURI (..))
import VoidTalon.Net.Completions (Update (..))
import qualified VoidTalon.Net.Completions as Completions
import VoidTalon.Net.Models (ModelInfo (id))
import qualified VoidTalon.TUI.Timeline as Timeline
import VoidTalon.TUI.Types
import qualified VoidTalon.Timeline as Timeline
import qualified VoidTalon.Util as Util
import VoidTalon.Util (remove)

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
    stopReason :: Maybe T.Text,
    -- | Index of the element in the timeline that's focused.  Starts from the bottom.
    timelineFocus :: Int
  }

makeLensesFor
  [ ("focus", "stateFocusL"),
    ("promptEditor", "statePromptEditorL"),
    ("timeline", "stateTimelineL"),
    ("stopReason", "stateStopReasonL"),
    ("timelineFocus", "stateTimelineFocusL")
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
        focus = focusRing [NPromptField, NTimelineVP],
        promptEditor = editorText NPromptField Nothing "",
        timeline = [],
        stopReason = Just "stop",
        timelineFocus = 0
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
              (barA, V.magenta `on` (V.Color240 $ 235 - 16)),
              (selectedA, fg V.blue)
            ]
    }

outputVPScroll :: ViewportScroll Name
outputVPScroll = viewportScroll NTimelineVP

draw :: State -> [Widget Name]
draw st = [vBox [output, hBorder, (joinBorders prompt), statusBar]]
  where
    output =
      Timeline.draw
        ( if (focusGetCurrent st.focus == Just NTimelineVP)
            then Just st.timelineFocus
            else Nothing
        )
        st.timeline
    prompt =
      withFocusRing
        st.focus
        (\b e -> vLimit 8 $ renderEditor (txt . T.unlines) b e)
        $ st.promptEditor
    statusBar = withAttr barA $ statusBarLeft <+> padLeft Max statusBarRight
    statusBarRight = txt $ fromMaybe "running" st.stopReason
    statusBarLeft = txt $ globalHelp <> localHelp

    globalHelp = "<C-q> Quit | <C-w> Focus | <C-e/y> Scl"
    localHelp = case focusGetCurrent st.focus of
      Just NPromptField -> " | <M-Cr> Ins. NL | <C-x> Editor"
      Just NTimelineVP -> " | k/j Move | d Delete | e Edit"
      _ -> ""

handleEvent :: BrickEvent Name Event -> EventM Name State ()
-- Exit with <C-q>
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
-- Scroll with <C-e> and <C-y>
handleEvent (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = vScrollBy outputVPScroll 1
handleEvent (VtyEvent (V.EvKey (V.KChar 'y') [V.MCtrl])) = vScrollBy outputVPScroll $ -1
-- Change focus with <C-w>
handleEvent (VtyEvent (V.EvKey (V.KChar 'w') [V.MCtrl])) = do
  stateFocusL %= focusNext
  cur <- focusGetCurrent <$> gets (.focus)
  -- If we've just focused the timeline, reset focused element to last entry
  when (cur == Just NTimelineVP) $ stateTimelineFocusL .= 0
  makeVisible $ NTimelineEntry 0
-- TODO: <> on ByteString is slow (O(n)), optimize
handleEvent (AppEvent (EvCompletionUpdate (UpdateMessage added))) = do
  -- append text to output
  stateTimelineL %= \case
    (Timeline.OutputEntry prev) : tl -> (Timeline.OutputEntry (prev <> added)) : tl
    tl -> (Timeline.OutputEntry added) : tl
  -- stick to bottom
  maybeVP <- lookupViewport NTimelineVP
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
  stateStopReasonL .= Just reason
handleEvent ev = do
  st <- get
  case focusGetCurrent $ st.focus of
    Just NPromptField -> case ev of
      -- I would prefer if this were shift+enter rather than meta+enter, but that doesn't seem to be
      -- supported by vty.
      (VtyEvent (V.EvKey V.KEnter [V.MMeta])) ->
        statePromptEditorL %= applyEdit breakLine
      (VtyEvent (V.EvKey V.KEnter [])) -> do
        let running = isNothing st.stopReason
        let prompt = T.intercalate "\n" $ getEditContents $ st.promptEditor
        unless (running || T.null prompt) $ do
          -- clear entry
          statePromptEditorL %= applyEdit clearZipper

          -- append prompt to timeline
          stateTimelineL %= (Timeline.PromptEntry prompt :)

          -- set runStatus to running
          stateStopReasonL .= Nothing

          -- start completions request
          ctx <- Completions.Context st.model.id . reverse <$> gets (.timeline)
          liftIO $
            Completions.perform
              ( writeBChan (st.evchan)
                  . EvCompletionUpdate
              )
              (st.config).connection.base_url.inner
              (st.httpMan)
              ctx

      -- Invoke editor with <C-x>
      (VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl])) -> do
        prompt <- gets $ LT.intercalate "\n" . fmap LT.fromStrict . getEditContents . (.promptEditor)
        suspendAndResume $ do
          prompt' <- Util.editInEditor "md" prompt <|> pure prompt
          let ls = LT.toStrict <$> LT.split (== '\n') prompt'
          pure $ st & statePromptEditorL %~ (applyEdit $ const $ textZipper ls Nothing)
      _ -> zoom statePromptEditorL $ handleEditorEvent ev
    Just NTimelineVP -> do
      cur <- gets (.timelineFocus)
      case ev of
        (VtyEvent (V.EvKey (V.KChar 'k') [])) -> do
          tl <- gets (.timeline)
          let new = case cur + 1 of
                n | n >= length tl -> 0
                n -> n
          stateTimelineFocusL .= new
          makeVisible $ NTimelineEntry new
        (VtyEvent (V.EvKey (V.KChar 'j') [])) -> do
          new <- case cur - 1 of
            n | n < 0 -> subtract 1 . length <$> gets (.timeline)
            n -> pure n
          stateTimelineFocusL .= new
          makeVisible $ NTimelineEntry new
        (VtyEvent (V.EvKey (V.KChar 'd') [])) -> do
          idx <- max 0 <$> gets (.timelineFocus)
          stateTimelineL %= remove idx
        (VtyEvent (V.EvKey (V.KChar 'e') [])) -> do
          idx <- gets (.timelineFocus)
          suspendAndResume $ mapMOf (stateTimelineL . ix idx) (liftIO . Timeline.editEntry) st
        _ -> pure ()
    _ -> pure ()
