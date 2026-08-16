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
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Edit
import Brick.Widgets.List (listSelectedAttr)
import Control.Applicative ((<|>))
import Control.Concurrent (killThread)
import Control.Exception (try)
import Control.Exception.Base (SomeException)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Zipper (breakLine, clearZipper, textZipper)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLensesFor)
import qualified Network.HTTP.Client as HTTP
import Text.Printf (printf)
import VoidTalon.Config (Config (..))
import VoidTalon.Net.Completions (Update (..))
import qualified VoidTalon.Net.Completions as Completions
import qualified VoidTalon.TUI.Help as Help
import qualified VoidTalon.TUI.ModelSelector as MS
import qualified VoidTalon.TUI.Timeline as Timeline
import qualified VoidTalon.TUI.ToolManager as TM
import VoidTalon.TUI.Types
import qualified VoidTalon.Timeline as Timeline
import qualified VoidTalon.Tools as Tools
import VoidTalon.Util (BufferedBChan, remove)
import qualified VoidTalon.Util as Util

data State = State
  { config :: Config,
    evchan :: BufferedBChan Event,
    focus :: FocusRing Name,
    promptEditor :: Editor T.Text Name,
    httpMan :: HTTP.Manager,
    timeline :: Timeline.State,
    models :: MS.Selector,
    -- | The last stop reason the server sent us.  We only consider completions done once the server
    -- finishes the request response, at which point this is transferred to runState.
    lastStopReason :: Maybe T.Text,
    runState :: RunState,
    stats :: Completions.TokenStats,
    -- | Tools that the model requested but the user hasn't reviewed yet.
    -- (id, name, invocation)
    pendingTools :: [(Tools.CallID, T.Text, Tools.Invocation)],
    openPopup :: Maybe Name,
    tools :: TM.Manager
  }

makeLensesFor
  [ ("focus", "stateFocusL"),
    ("promptEditor", "statePromptEditorL"),
    ("timeline", "stateTimelineL"),
    ("models", "stateModelsL"),
    ("lastStopReason", "stateLastStopReasonL"),
    ("runState", "stateRunStateL"),
    ("stats", "stateStatsL"),
    ("pendingTools", "statePendingToolsL"),
    ("openPopup", "stateOpenPopupL"),
    ("tools", "stateToolsL")
  ]
  ''State

mkInitialState ::
  Config ->
  BufferedBChan Event ->
  HTTP.Manager ->
  Maybe T.Text ->
  [(T.Text, Tools.Tool)] ->
  IO State
mkInitialState config evchan httpMan model tools =
  pure $
    State
      { config,
        evchan,
        httpMan,
        models = MS.newSelector model,
        focus = focusRing [NPromptField, NTimelineVP],
        promptEditor = editorText NPromptField Nothing "",
        timeline = Timeline.initialState,
        lastStopReason = Nothing,
        runState = RunStateStopped "stop",
        stats = Completions.emptyStats,
        pendingTools = [],
        openPopup = Nothing,
        tools = TM.newManager tools
      }

type App' = App State [Event] Name

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
            [ (listSelectedAttr, (bg (V.Color240 $ 240 - 16))),
              (warningA, fg V.yellow),
              (barA, V.magenta `on` (V.Color240 $ 235 - 16)),
              (selectedA, fg V.blue),
              (toolTitleA, (fg V.red) {V.attrStyle = V.SetTo V.bold}),
              (toolResultBorderA, fg V.red),
              (toolPlanHeaderA, (fg V.magenta) {V.attrStyle = V.SetTo V.bold}),
              (toolManagerToolTitleA, (fg V.cyan) {V.attrStyle = V.SetTo V.bold}),
              ( toolManagerSchemaTypeA,
                (V.red `on` (V.Color240 $ 235 - 16))
                  { V.attrStyle = V.SetTo V.bold
                  }
              ),
              (toolManagerSchemaKeyA, fg V.green)
            ]
    }

effectiveFocus :: State -> Maybe Name
effectiveFocus st =
  (listToMaybe st.pendingTools >> Just NToolDialog)
    <|> st.openPopup
    <|> (focusGetCurrent st.focus)

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
        ++ case st.openPopup of
          Just NToolManager -> pure . showPopup "Manage Tools" $ TM.draw st.tools
          Just NHelp -> pure . showPopup "Help" $ Help.draw
          Just NModelSelector -> pure . showPopup "Models" $ MS.draw st.models
          Just _ -> undefined -- invalid state
          _ -> []
    output =
      Timeline.draw
        (effectiveFocus st == Just NTimelineVP)
        st.timeline
    prompt =
      withFocusRing
        st.focus
        (\b e -> vLimit 8 $ renderEditor (txt . T.unlines) b e)
        $ st.promptEditor
    statusBar = withAttr barA (statusBarLeft <+> padLeft Max statusBarRight)
    statusBarLeft = txt $ fromMaybe "<no model>" st.models.active
    statusBarRight =
      txt $
        let run = case st.runState of
              RunStateStopped reason -> reason
              RunStateRunning _ -> "running"
            Completions.TokenStats {tps, nCompletion, nPrompt} = st.stats
         in mconcat
              [ T.show nPrompt,
                " in ",
                T.show nCompletion,
                " out ",
                T.pack $ printf "%.2f" tps,
                "/s ",
                run
              ]
    showPopup name widget =
      centerLayer
        . hLimitPercent overlaySizeLimitPercent
        . vLimitPercent overlaySizeLimitPercent
        . borderWithLabel (txt name)
        $ widget

handleEvent :: BrickEvent Name [Event] -> EventM Name State ()
-- Exit with <C-q>
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
-- Scroll with <C-e> and <C-y>
handleEvent (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = vScrollBy Timeline.outputVPScroll 1
handleEvent (VtyEvent (V.EvKey (V.KChar 'y') [V.MCtrl])) = vScrollBy Timeline.outputVPScroll $ -1
-- Change focus with <C-w>
handleEvent (VtyEvent (V.EvKey (V.KChar 'w') [V.MCtrl])) = do
  stateFocusL %= focusNext
  cur <- focusGetCurrent <$> gets (.focus)
  -- If we've just focused the timeline, reset focused element to last entry
  when (cur == Just NTimelineVP) $ do
    stateTimelineL . Timeline.stateFocusL .= 0
    makeVisible $ NTimelineEntry 0
  -- When we change timeline focus, we invalidate the whole cache.  This is because calculating
  -- which element has LOST focus due to the focus change is too error-prone and not worth the risk.
  invalidateCache
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
handleEvent (VtyEvent (V.EvKey (V.KFun 1) [])) = openPopup NHelp
handleEvent (VtyEvent (V.EvKey (V.KChar 't') [V.MCtrl])) = openPopup NToolManager
handleEvent (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) = do
  st <- get
  openPopup NModelSelector
  zoom stateModelsL $ MS.onOpened st.config.connection st.httpMan st.evchan
handleEvent (AppEvent evs) = sequence_ $ handleAppEvent <$> reverse evs
handleEvent ev = do
  st <- get
  let popupCtx = PopupContext {config = st.config, httpMan = st.httpMan, evchan = st.evchan}
  case effectiveFocus st of
    Just NPromptField -> case ev of
      -- I would prefer if this were shift+enter rather than meta+enter, but that doesn't seem to be
      -- supported by vty.
      (VtyEvent (V.EvKey V.KEnter [V.MMeta])) ->
        statePromptEditorL %= applyEdit breakLine
      (VtyEvent (V.EvKey V.KEnter [])) -> do
        let prompt = T.intercalate "\n" $ getEditContents $ st.promptEditor
        unless (T.null prompt) $ do
          -- append prompt to timeline
          zoom stateTimelineL $ do
            Timeline.stateEntriesL %= (Timeline.PromptEntry prompt :)
            Timeline.stickToBottom

          -- clear entry
          statePromptEditorL %= applyEdit clearZipper

          invalidateCache

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
        let new = Util.focusAdd (length st.timeline.entries) st.timeline.focus
        stateTimelineL . Timeline.stateFocusL .= new
        makeVisible $ NTimelineEntry new
        invalidateCache
      (VtyEvent (V.EvKey (V.KChar 'j') [])) -> do
        let new = Util.focusSub (length st.timeline.entries) st.timeline.focus
        stateTimelineL . Timeline.stateFocusL .= new
        makeVisible $ NTimelineEntry new
        invalidateCache
      (VtyEvent (V.EvKey (V.KChar 'd') [])) -> do
        let idx = max 0 (st.timeline.focus)
        stateTimelineL . Timeline.stateEntriesL %= remove idx
        -- We need to invalidate the cache because indices just shifted.
        invalidateCache
      (VtyEvent (V.EvKey (V.KChar 'e') [])) -> do
        let focus = st.timeline.focus
        suspendAndResume $
          mapMOf
            (stateTimelineL . Timeline.stateEntriesL . ix focus)
            (liftIO . Timeline.editEntry)
            st
        invalidateCacheEntry $ NTimelineEntry focus
      (VtyEvent (V.EvKey V.KEnter [])) -> when (isStopped st.runState) $ do
        -- tail of the timeline with the selected entry being the last one
        let tl' = drop st.timeline.focus $ st.timeline.entries
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
          stateTimelineL . Timeline.stateEntriesL .= tl
          startCompletions
        invalidateCache
      _ -> pure ()
    Just NToolManager -> zoom stateToolsL $ TM.handleEvent popupCtx ev
    Just NHelp -> Help.handleEvent popupCtx ev
    Just NModelSelector -> zoom stateModelsL $ MS.handleEvent popupCtx ev
    Just NToolDialog ->
      let finishTool ::
            Tools.CallID ->
            T.Text ->
            [(Maybe T.Text, T.Text, Tools.Invocation)] ->
            EventM Name State ()
          finishTool id' content rest = do
            let entry = Timeline.ToolResultEntry {id = id', content = content}
            zoom stateTimelineL $ do
              Timeline.stateEntriesL %= (entry :)
              Timeline.stickToBottom
            invalidateCache
            statePendingToolsL .= rest
            when (null rest) $ startCompletions
       in case (st.pendingTools, ev) of
            ((id', _, (_, invoke)) : rest, VtyEvent (V.EvKey (V.KChar 'y') [])) -> do
              result <-
                suspendAndResume' $
                  try invoke <&> \case
                    Left err -> T.pack $ "Error: " <> (show (err :: SomeException))
                    Right res -> Tools.postProcessToolOutput res
              finishTool id' result rest
            ((id', _, _) : rest, VtyEvent (V.EvKey (V.KChar 'n') [])) ->
              finishTool id' "Error: user denied tool invocation" rest
            ((id', _, _) : rest, VtyEvent (V.EvKey (V.KChar 's') [])) -> do
              result <- suspendAndResume' (Util.editInEditor "md" LT.empty)
              finishTool id' (LT.toStrict result) rest
            _ -> pure ()
    _ -> pure ()

-- | Starts generation using the current timeline, if possible.
-- If no model is selected, does nothing.
startCompletions :: EventM n State ()
startCompletions = do
  st <- get
  case st.models.active of
    Just m | (isStopped st.runState) -> do
      let ctx =
            Completions.Context
              { model = m,
                timeline = reverse st.timeline.entries,
                tools = TM.activeTools st.tools
              }
      -- start completions request
      thread <-
        liftIO $
          Completions.perform
            (flip Util.writeBufferedBChan (st.evchan) . EvCompletionUpdate)
            (Util.blockWriteBufferedBChan EvCompletionDone st.evchan)
            st.config.connection
            st.httpMan
            ctx

      -- set runStatus to running
      stateRunStateL .= RunStateRunning thread
    _ -> pure ()

handleAppEvent :: Event -> EventM Name State ()
-- TODO: <> on ByteString is slow (O(n)), optimize
handleAppEvent (EvCompletionUpdate (UpdateMessage added stats)) = do
  stateStatsL .= stats
  zoom stateTimelineL $ do
    -- append text to output
    ents <- gets (.entries)
    case ents of
      (Timeline.OutputEntry prev) : tl -> do
        Timeline.stateEntriesL .= (Timeline.OutputEntry (prev <> added)) : tl
        -- We modified the last timeline entry, i.e. the one with index 0.
        invalidateCacheEntry $ NTimelineEntry 0
      tl -> do
        Timeline.stateEntriesL .= (Timeline.OutputEntry added) : tl
        -- We added a new timeline entry, shifting indices.  Invalidate entire cache.
        invalidateCache
    Timeline.stickToBottom
handleAppEvent (EvCompletionUpdate (UpdateStop reason)) =
  stateLastStopReasonL .= Just reason
handleAppEvent EvCompletionDone = do
  st <- get
  stateLastStopReasonL .= Nothing
  stateRunStateL %= \case
    s@(RunStateStopped _) ->
      -- if we're already stopped, probably due to the user cancelling the completions, keep the
      -- reason.
      s
    RunStateRunning _ -> (RunStateStopped (fromMaybe "<unknown stop>" st.lastStopReason))
  case st.timeline.entries of
    ((Timeline.OutputEntry Timeline.LLMMessage {toolCalls}) : _) -> do
      let (brokenCalls, calls) = partitionEithers $ prepareInvocation st <$> IntMap.elems toolCalls
      -- Append broken calls to timeline right away
      zoom stateTimelineL $ do
        Timeline.stateEntriesL
          %= ( ( uncurry Timeline.ToolResultEntry
                   . (("Error: " <>) . T.pack <$>)
                   <$> brokenCalls
               )
                 ++
             )
        Timeline.stickToBottom
      -- Schedule valid calls for review
      statePendingToolsL .= calls
      invalidateCache -- index shift
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
handleAppEvent EvClosePopup =
  stateOpenPopupL .= Nothing
handleAppEvent (EvModelList ms) = zoom stateModelsL $ MS.modelsReceived ms

openPopup :: Name -> EventM n State ()
openPopup n = stateOpenPopupL %= (<|> Just n)
