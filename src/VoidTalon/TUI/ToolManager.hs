{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI.ToolManager
  ( Manager,
    newManager,
    activeTools,
    findTool,
    handleEvent,
    draw,
    helpText,
  )
where

import Brick
import Data.Function (applyWhen)
import Data.List (find)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLensesFor)
import VoidTalon.TUI.Types (toolManagerSelectedA, toolManagerToolTitleA)
import qualified VoidTalon.Tools as Tools

-- | Current state of a registered tool.
-- (enabled, name, tool)
type ToolState = (Bool, T.Text, Tools.Tool)

data Manager = Manager
  { states :: ![ToolState],
    selected :: Int
  }

makeLensesFor
  [ ("states", "managerStatesL"),
    ("selected", "managerSelectedL")
  ]
  ''Manager

-- | Create a new manager that knows about the given tools
newManager :: [(T.Text, Tools.Tool)] -> Manager
newManager tools =
  Manager
    { states = (\(name, tool) -> (False, name, tool)) <$> tools,
      selected = 0
    }

-- | For each active tool, gets that tool's name and description.  This can then be used to create a
-- completions context directly.
activeTools :: Manager -> [(T.Text, Tools.Description)]
activeTools Manager {states} =
  (\(_, name, tool) -> (name, tool.description))
    <$> filter (\(enabled, _, _) -> enabled) states

-- | Searches the currently active tools for one of the given name.
findTool :: Manager -> T.Text -> Maybe Tools.Tool
findTool Manager {states} name =
  (\(_, _, t) -> t) <$> find (\(enabled, n, _) -> enabled && n == name) states

handleEvent :: BrickEvent n e -> EventM n Manager ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = do
  m <- get
  managerSelectedL .= case m.selected + 1 of
    n | n > length m.states -> 0
    n -> n
handleEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = do
  m <- get
  managerSelectedL .= case m.selected - 1 of
    n | n < 0 -> length m.states - 1
    n -> n
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  sel <- gets (.selected)
  managerStatesL . ix sel . _1 %= not
handleEvent _ = pure ()

draw :: Manager -> Widget n
draw Manager {states, selected} =
  vBox $
    (\(s, i) -> drawState (i == selected) s) <$> zip states [0 ..]

drawState :: Bool -> ToolState -> Widget n
drawState selected (enabled, name, Tools.Tool {description = Tools.Description {description}}) =
  vBox
    [ withAttr (applyWhen selected (<> toolManagerSelectedA) toolManagerToolTitleA) $
        txt (check <> name),
      txtWrap description
    ]
  where
    check = if enabled then "● " else "○ "

helpText :: T.Text
helpText = " | k/j Move | <Space> Toggle"
