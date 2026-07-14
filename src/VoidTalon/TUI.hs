{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module VoidTalon.TUI
  ( app,
    mkInitialState,
    State (..),
    Event (..),
    module VoidTalon.TUI.Types,
  )
where

import Brick
import Brick.BChan
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Text as T
import Data.Text.Zipper (clearZipper)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH (makeLensesFor)
import qualified Network.HTTP.Client as HTTP
import VoidTalon.Config (Config (..), ConnectionConfig (..), TomlURI (..))
import qualified VoidTalon.Net.Completions as Completions
import VoidTalon.TUI.Types
import qualified VoidTalon.TUI.Timeline as Timeline

-- | Number of lines our prompt will have at most.
promptLines :: Maybe Int
promptLines = Just 8

data State = State
  { config :: Config,
    evchan :: BChan Event,
    focus :: FocusRing Name,
    promptEditor :: Editor T.Text Name,
    running :: IORef Bool,
    httpMan :: HTTP.Manager,
    -- | These are stored in reverse since we update the latest message.
    timeline :: [Timeline.Entry]
  }

makeLensesFor
  [ ("promptEditor", "statePromptEditorL"),
    ("timeline", "stateTimelineL")
  ]
  ''State

mkInitialState :: Config -> BChan Event -> IO State
mkInitialState config evchan = do
  running' <- newIORef False
  httpMan' <- HTTP.newManager HTTP.defaultManagerSettings
  pure $
    State
      { config,
        evchan,
        focus = focusRing [NPromptField],
        promptEditor = editorText NPromptField promptLines "",
        running = running',
        httpMan = httpMan',
        timeline = []
      }

type App' = App State Event Name

app :: App'
app =
  App
    { appDraw = draw,
      appChooseCursor = focusRingCursor (.focus),
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ attrMap V.defAttr []
    }

outputVPScroll :: ViewportScroll Name
outputVPScroll = viewportScroll NOutputVP

draw :: State -> [Widget Name]
draw st = [vBox [output, hBorder, (joinBorders prompt)]]
  where
    output = withVScrollBars OnRight
      $ viewport NOutputVP Vertical
      $ vBox
      $ foldl (flip $ (:) . Timeline.entryWidget) [] st.timeline
    prompt = withFocusRing st.focus (renderEditor (txt . T.unlines)) $ st.promptEditor

handleEvent :: BrickEvent Name Event -> EventM Name State ()
-- Exit with <C-q>
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
-- Scroll with <C-e> and <C-y>
handleEvent (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = vScrollBy outputVPScroll 1
handleEvent (VtyEvent (V.EvKey (V.KChar 'y') [V.MCtrl])) = vScrollBy outputVPScroll $ -1
-- TODO: <> on ByteString is slow (O(n)), optimize
handleEvent (AppEvent (EvGetCompletions added)) = do
  -- append text to output
  zoom stateTimelineL $ modify $ \case
    (Timeline.OutputEntry prev):tl -> (Timeline.OutputEntry (prev <> added)):tl
    tl -> (Timeline.OutputEntry added):tl
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
handleEvent ev = do
  st <- get
  case focusGetCurrent $ st.focus of
    Just NPromptField -> case ev of
      -- TODO: we don't get shift in the modifiers here when the user presses shift-enter!  Bad!
      (VtyEvent (V.EvKey V.KEnter [])) -> do
        running' <- liftIO $ readIORef $ st.running
        when (not running') $ do
          -- clear entry
          zoom statePromptEditorL $ modify $ applyEdit clearZipper
          let prompt = mconcat $ getEditContents $ st.promptEditor
          -- append prompt to timeline
          zoom stateTimelineL $ modify (Timeline.PromptEntry prompt :)
          -- start completions request
          let ctx =
                Completions.Context
                  { Completions.prompt = prompt
                  }
          liftIO $
            Completions.perform
              ( writeBChan (st.evchan)
                  . EvGetCompletions
                  . (.delta)
              )
              (st.config).connection.base_url.inner
              (st.httpMan)
              (st.running)
              ctx
      _ -> zoom statePromptEditorL $ handleEditorEvent ev
    _ -> pure ()
