{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI (app, mkInitialState, App', State (..), Event (..)) where

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
import Lens.Micro.TH
import qualified Network.HTTP.Client as HTTP
import VoidTalon.Config (Config (connection), ConnectionConfig (base_url), TomlURI (getTomlURI))
import qualified VoidTalon.Net.Completions as Completions

data Event = EvGetText T.Text

data Name = NPromptField | NOutputVP deriving (Eq, Ord, Show)

data State = State
  { _config :: Config,
    _evchan :: BChan Event,
    _focus :: FocusRing Name,
    _outputText :: T.Text,
    _promptEditor :: Editor T.Text Name,
    _running :: IORef Bool,
    _httpMan :: HTTP.Manager
  }

makeLenses ''State

-- | Number of lines our prompt will have at most.
promptLines :: Maybe Int
promptLines = Just 8

mkInitialState :: Config -> BChan Event -> IO State
mkInitialState _config _evchan = do
  running' <- newIORef False
  httpMan' <- HTTP.newManager HTTP.defaultManagerSettings
  pure $
    State
      { _config,
        _evchan,
        _focus = focusRing [NPromptField],
        _outputText = "",
        _promptEditor = editorText NPromptField promptLines "",
        _running = running',
        _httpMan = httpMan'
      }

type App' = App State Event Name

app :: App'
app =
  App
    { appDraw = draw,
      appChooseCursor = focusRingCursor (^. focus),
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ attrMap V.defAttr []
    }

outputVPScroll :: ViewportScroll Name
outputVPScroll = viewportScroll NOutputVP

draw :: State -> [Widget Name]
draw st = [vBox [output, hBorder, (joinBorders prompt)]]
  where
    output = withVScrollBars OnRight $ viewport NOutputVP Vertical $ txtWrap $ st ^. outputText
    prompt = withFocusRing (st ^. focus) (renderEditor (txt . T.unlines)) $ st ^. promptEditor

handleEvent :: BrickEvent Name Event -> EventM Name State ()
-- Exit with <C-q>
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
-- Scroll with <C-e> and <C-y>
handleEvent (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = vScrollBy outputVPScroll 1
handleEvent (VtyEvent (V.EvKey (V.KChar 'y') [V.MCtrl])) = vScrollBy outputVPScroll $ -1
-- TODO: <> on ByteString is slow (O(n)), optimize
handleEvent (AppEvent (EvGetText added)) = do
  -- append text to output
  zoom outputText $ modify (<> added)
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
  case focusGetCurrent $ st ^. focus of
    Just NPromptField -> zoom promptEditor $ case ev of
      -- TODO: we don't get shift in the modifiers here when the user presses shift-enter!  Bad!
      (VtyEvent (V.EvKey V.KEnter [])) -> do
        running' <- liftIO $ readIORef $ st ^. running
        when (not running') $ do
          modify $ applyEdit clearZipper
          let ctx =
                Completions.Context
                  { Completions.prompt = mconcat $ getEditContents $ st ^. promptEditor
                  }
          liftIO $
            Completions.perform
              ( writeBChan (st ^. evchan)
                  . EvGetText
                  . (\(Completions.CompletionChoice dr dc) -> dr <> dc)
                  . (.delta)
              )
              (st ^. config).connection.base_url.getTomlURI
              (st ^. httpMan)
              (st ^. running)
              ctx
      _ -> handleEditorEvent ev
    _ -> pure ()
