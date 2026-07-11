{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (app, mkInitialState, App', State (..), Event (..), Name (..)) where

import Brick
import Brick.BChan (BChan)
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Config (Config)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text.Zipper (clearZipper)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

data Event = PlaceholderEvent

data Name = PromptField deriving (Eq, Ord, Show)

data State = State
  { _config :: Config,
    _evchan :: BChan Event,
    _focus :: FocusRing Name,
    _outputText :: T.Text,
    _promptEditor :: Editor T.Text Name
  }

makeLenses ''State

-- | Number of lines our prompt will have at most.
promptLines :: Maybe Int
promptLines = Just 8

mkInitialState :: Config -> BChan Event -> State
mkInitialState _config _evchan =
  State
    { _config,
      _evchan,
      _focus = focusRing [PromptField],
      _outputText = "test",
      _promptEditor = editorText PromptField promptLines ""
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
    output = txtWrap $ st ^. outputText
    prompt = withFocusRing (st ^. focus) (renderEditor (txt . T.unlines)) $ st ^. promptEditor

handleEvent :: BrickEvent Name Event -> EventM Name State ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
handleEvent ev = do
  st <- get
  case focusGetCurrent $ st ^. focus of
    Just PromptField -> zoom promptEditor $ case ev of
      -- TODO: we don't get shift in the modifiers here when the user presses shift-enter!  Bad!
      (VtyEvent (V.EvKey V.KEnter [])) -> modify $ applyEdit clearZipper
      _ -> handleEditorEvent ev
    _ -> pure ()
