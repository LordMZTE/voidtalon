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
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Edit
import Control.Applicative ((<|>))
import Control.Concurrent (killThread)
import Control.Exception (try)
import Control.Exception.Base (SomeException)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as IntMap
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
import qualified VoidTalon.TUI.ToolManager as TM
import VoidTalon.TUI.Types
import qualified VoidTalon.Timeline as Timeline
import qualified VoidTalon.Tools as Tools
import qualified VoidTalon.Tools.ReadFile
import VoidTalon.Util (remove)
import qualified VoidTalon.Util as Util

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
    runState :: RunState,
    -- | Index of the element in the timeline that's focused.  Starts from the bottom.
    timelineFocus :: Int,
    -- | Tools that the model requested but the user hasn't reviewed yet.
    -- (id, name, invocation)
    pendingTools :: [(Tools.CallID, T.Text, Tools.Invocation)],
    tools :: TM.Manager
  }

makeLensesFor
  [ ("focus", "stateFocusL"),
    ("promptEditor", "statePromptEditorL"),
    ("timeline", "stateTimelineL"),
    ("runState", "stateRunStateL"),
    ("timelineFocus", "stateTimelineFocusL"),
    ("pendingTools", "statePendingToolsL"),
    ("tools", "stateToolsL")
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
        focus = focusRing [NPromptField, NTimelineVP, NToolManager],
        promptEditor = editorText NPromptField Nothing "",
        timeline = [],
        runState = RunStateStopped "stop",
        timelineFocus = 0,
        pendingTools = [],
        tools
      }
  where
    tools = TM.newManager [("read_file", VoidTalon.Tools.ReadFile.tool)]

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
              (selectedA, fg V.blue),
              (toolTitleA, (fg V.red) {V.attrStyle = V.SetTo V.bold}),
              (toolResultBorderA, fg V.red),
              (toolPlanHeaderA, (fg V.magenta) {V.attrStyle = V.SetTo V.bold}),
              (toolManagerToolTitleA, (fg V.cyan) {V.attrStyle = V.SetTo V.bold}),
              (toolManagerSelectedA, (bg (V.Color240 $ 240 - 16)))
            ]
    }

outputVPScroll :: ViewportScroll Name
outputVPScroll = viewportScroll NTimelineVP

effectiveFocus :: State -> Maybe Name
effectiveFocus st = if null st.pendingTools then focusGetCurrent st.focus else Just NToolDialog

draw :: State -> [Widget Name]
draw st = overlays ++ [vBox [output, hBorder, (joinBorders prompt), statusBar]]
  where
    overlays =
      case st.pendingTools of
        (_, name, (plan, _)) : _ ->
          let entry hdr t = [withAttr toolPlanHeaderA $ txtWrap hdr, border $ txtWrap t]
              boxWidgets = concatMap (uncurry entry) plan
           in pure
                . centerLayer
                . hLimitPercent overlaySizeLimitPercent
                . borderWithLabel (txt "Tool Call Request")
                . vBox
                $ (withAttr toolTitleA $ txtWrap $ "LLM Requested to call " <> name)
                  : (txt T.empty) -- empty line for spacing
                  : boxWidgets
        _ -> []
        -- intentionally not @effectiveFocus@ to render overlays even when they're not focused.
        ++ case focusGetCurrent st.focus of
          Just NToolManager ->
            pure
              . centerLayer
              . hLimitPercent overlaySizeLimitPercent
              . vLimitPercent overlaySizeLimitPercent
              . borderWithLabel (txt "Manage Tools")
              $ TM.draw st.tools
          _ -> []
    output =
      Timeline.draw
        ( if (effectiveFocus st == Just NTimelineVP)
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
    statusBarRight = txt $ case st.runState of
      RunStateStopped reason -> reason
      RunStateRunning _ -> "running"
    statusBarLeft = txt $ globalHelp <> localHelp

    globalHelp = "<C-q> Quit | <C-w> Focus | <C-e/y> Scl | <C-c> Cancel"
    localHelp =
      case effectiveFocus st of
        Just NPromptField -> " | <M-Cr> Ins. NL | <C-x> Editor"
        Just NTimelineVP -> " | k/j Move | d Delete | e Edit | <CR> Regen"
        Just NToolDialog -> " | y Confirm | n Deny | s Spoof"
        Just NToolManager -> TM.helpText
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
-- Stop ongoing completions with <C-c>
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = do
  st <- get
  case st.runState of
    RunStateStopped _ ->
      unless (null st.pendingTools) $
        statePendingToolsL .= []
    RunStateRunning thread -> do
      liftIO $ killThread thread
      stateRunStateL .= RunStateStopped "cancelled"
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
handleEvent (AppEvent (EvCompletionUpdate (UpdateStop reason))) = do
  stateRunStateL .= RunStateStopped reason
  st <- get
  case st.timeline of
    ((Timeline.OutputEntry Timeline.LLMMessage {toolCalls}) : _) -> do
      let (brokenCalls, calls) = partitionEithers $ prepareInvocation st <$> IntMap.elems toolCalls
      -- Append broken calls to timeline right away
      stateTimelineL
        %= ( ( uncurry Timeline.ToolResultEntry
                 . (("Error: " <>) . T.pack <$>)
                 <$> brokenCalls
             )
               ++
           )
      -- Schedule valid calls for review
      statePendingToolsL .= calls
    _ -> pure ()
  where
    prepareInvocation ::
      State ->
      Tools.Call ->
      Either (Tools.CallID, String) (Tools.CallID, T.Text, Tools.Invocation)
    prepareInvocation st Tools.Call {id = id', name, parameters} =
      case TM.findTool st.tools name of
        Just (Tools.Tool {invoke}) -> case invoke parameters of
          Left err -> Left (id', err)
          Right res -> Right (id', name, res)
        Nothing -> Left (id', "No such tool exists")
handleEvent ev = do
  st <- get
  case effectiveFocus st of
    Just NPromptField -> case ev of
      -- I would prefer if this were shift+enter rather than meta+enter, but that doesn't seem to be
      -- supported by vty.
      (VtyEvent (V.EvKey V.KEnter [V.MMeta])) ->
        statePromptEditorL %= applyEdit breakLine
      (VtyEvent (V.EvKey V.KEnter [])) -> do
        let running = isRunning st.runState
        let prompt = T.intercalate "\n" $ getEditContents $ st.promptEditor
        unless (running || T.null prompt) $ do
          -- clear entry
          statePromptEditorL %= applyEdit clearZipper

          -- append prompt to timeline
          stateTimelineL %= (Timeline.PromptEntry prompt :)

          startCompletions

      -- Invoke editor with <C-x>
      (VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl])) -> do
        let prompt = LT.intercalate "\n" . fmap LT.fromStrict . getEditContents $ st.promptEditor
        suspendAndResume $ do
          prompt' <- Util.editInEditor "md" prompt <|> pure prompt
          let ls = LT.toStrict <$> LT.split (== '\n') prompt'
          pure $ st & statePromptEditorL %~ (applyEdit $ const $ textZipper ls Nothing)
      _ -> zoom statePromptEditorL $ handleEditorEvent ev
    Just NTimelineVP -> case ev of
      (VtyEvent (V.EvKey (V.KChar 'k') [])) -> do
        let new = case st.timelineFocus + 1 of
              n | n >= length st.timeline -> 0
              n -> n
        stateTimelineFocusL .= new
        makeVisible $ NTimelineEntry new
      (VtyEvent (V.EvKey (V.KChar 'j') [])) -> do
        let new = case st.timelineFocus - 1 of
              n | n < 0 -> subtract 1 $ length st.timeline
              n -> n
        stateTimelineFocusL .= new
        makeVisible $ NTimelineEntry new
      (VtyEvent (V.EvKey (V.KChar 'd') [])) -> do
        let idx = max 0 (st.timelineFocus)
        stateTimelineL %= remove idx
      (VtyEvent (V.EvKey (V.KChar 'e') [])) -> do
        suspendAndResume $
          mapMOf
            (stateTimelineL . ix st.timelineFocus)
            (liftIO . Timeline.editEntry)
            st
      (VtyEvent (V.EvKey V.KEnter [])) -> when (isStopped st.runState) $ do
        -- tail of the timeline with the selected entry being the last one
        let tl' = drop st.timelineFocus $ st.timeline
        let tl =
              -- Remove timeline entries from our tail until a prompt or tool result remains
              dropWhile
                ( \case
                    (Timeline.OutputEntry _) -> True
                    _ -> False
                )
                tl'

        -- If the rest the timeline is completely empty, we have nothing to work with.
        unless (null tl) $ do
          stateTimelineL .= tl
          startCompletions
      _ -> pure ()
    Just NToolManager -> zoom stateToolsL $ TM.handleEvent ev
    Just NToolDialog ->
      let finishTool ::
            Tools.CallID ->
            T.Text ->
            [(Maybe T.Text, T.Text, Tools.Invocation)] ->
            EventM n State ()
          finishTool id' content rest = do
            let entry = Timeline.ToolResultEntry {id = id', content = content}
            stateTimelineL %= (entry :)
            statePendingToolsL .= rest
            when (null rest && isStopped st.runState) startCompletions
       in case (st.pendingTools, ev) of
            ((id', _, (_, invoke)) : rest, VtyEvent (V.EvKey (V.KChar 'y') [])) -> do
              result <-
                suspendAndResume' $
                  try invoke <&> \case
                    Left err -> T.pack $ "Error: " <> (show (err :: SomeException))
                    Right res -> res
              finishTool id' result rest
            ((id', _, _) : rest, VtyEvent (V.EvKey (V.KChar 'n') [])) ->
              finishTool id' "Error: user denied tool invocation" rest
            ((id', _, _) : rest, VtyEvent (V.EvKey (V.KChar 's') [])) -> do
              result <- suspendAndResume' (Util.editInEditor "md" LT.empty)
              finishTool id' (LT.toStrict result) rest
            _ -> pure ()
    _ -> pure ()

-- | Starts generation using the current timeline.
-- Caller asserts that completions aren't already running.
startCompletions :: EventM n State ()
startCompletions = do
  st <- get
  let ctx =
        Completions.Context
          { model = st.model.id,
            timeline = reverse st.timeline,
            tools = TM.activeTools st.tools
          }
  -- start completions request
  thread <-
    liftIO $
      Completions.perform
        ( writeBChan (st.evchan)
            . EvCompletionUpdate
        )
        (st.config).connection.base_url.inner
        (st.httpMan)
        ctx

  -- set runStatus to running
  stateRunStateL .= RunStateRunning thread
