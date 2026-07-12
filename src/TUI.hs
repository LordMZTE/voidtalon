{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (app, mkInitialState, App', State (..), Event (..)) where

import Brick
import Brick.BChan
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Config (Config (connection), ConnectionConfig (base_url), TomlURI (getTomlURI))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Text as T
import Data.Text.Zipper (clearZipper)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH
import qualified Net.Completions
import Net.Completions (Context (..))
import qualified Network.HTTP.Client as HTTP

data Event = EvGetText T.Text

data Name = NPromptField | NOutputViewport deriving (Eq, Ord, Show)

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

draw :: State -> [Widget Name]
draw st = [vBox [output, hBorder, (joinBorders prompt)]]
  where
    output = withVScrollBars OnRight $ viewport NOutputViewport Vertical $ txtWrap $ st ^. outputText
    prompt = withFocusRing (st ^. focus) (renderEditor (txt . T.unlines)) $ st ^. promptEditor

handleEvent :: BrickEvent Name Event -> EventM Name State ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
-- TODO: <> on ByteString is slow (O(n)), optimize
handleEvent (AppEvent (EvGetText added)) = zoom outputText $ modify (<> added)
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
                Context
                  { prompt = mconcat $ getEditContents $ st ^. promptEditor
                  }
          liftIO $
            Net.Completions.perform
              (writeBChan (st ^. evchan) . EvGetText . (.deltaContent) . (.delta))
              (st ^. config).connection.base_url.getTomlURI
              (st ^. httpMan)
              (st ^. running)
              ctx
      _ -> handleEditorEvent ev
    _ -> pure ()
