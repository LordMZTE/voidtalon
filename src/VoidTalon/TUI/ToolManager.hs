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
import Brick.Widgets.Border (vBorder)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AK
import Data.Foldable (toList)
import Data.Function (applyWhen)
import Data.List (find, (!?))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLensesFor)
import VoidTalon.JSON (Schema (..), SchemaType)
import VoidTalon.TUI.Types
  ( Name (NToolManagerEntry, NToolManagerVP),
    PopupContext (..),
    toolManagerSchemaKeyA,
    toolManagerSchemaTypeA,
    toolManagerSelectedA,
    toolManagerToolTitleA,
  )
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

handleEvent :: PopupContext -> BrickEvent Name e -> EventM Name Manager ()
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'j') [])) = do
  m <- get
  let sel = case m.selected + 1 of
        n | n >= length m.states -> 0
        n -> n
  managerSelectedL .= sel
  makeVisible $ NToolManagerEntry sel
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'k') [])) = do
  m <- get
  let sel = case m.selected - 1 of
        n | n < 0 -> length m.states - 1
        n -> n
  managerSelectedL .= sel
  makeVisible $ NToolManagerEntry sel
handleEvent _ (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  sel <- gets (.selected)
  managerStatesL . ix sel . _1 %= not
handleEvent PopupContext {close} (VtyEvent (V.EvKey V.KEsc [])) =
  liftIO $ close
handleEvent _ _ = pure ()

draw :: Manager -> Widget Name
draw Manager {states, selected} = hBox [list, vBorder, schemaView]
  where
    list =
      withVScrollBars OnLeft
        . viewport NToolManagerVP Vertical
        . vBox
        $ ( \(s, i) ->
              reportExtent (NToolManagerEntry i) $
                drawState (i == selected) s
          )
          <$> zip states [0 ..]
    schemaView = case states !? selected of
      Just (_, _, Tools.Tool {description}) -> schemaWidget description.schema
      Nothing -> emptyWidget

    typeWidget :: [SchemaType] -> Widget n
    typeWidget =
      withAttr toolManagerSchemaTypeA
        . txt
        . (\t -> mconcat ["[", t, "]"])
        . T.intercalate "/"
        . (T.show <$>)

    schemaWidget :: Either Schema J.Value -> Widget n
    schemaWidget (Left Schema {types, properties, required, description}) =
      if null properties then top else top <=> (padLeft (Pad 2) $ vBox $ propertyWidget <$> properties)
      where
        top =
          typeWidget types
            <+> padLeft
              (Pad 1)
              (txtWrap $ fromMaybe "<no description>" description)
        propertyWidget (name, schema) =
          txt (name <> if elem name required then ": " else "?: ")
            <+> schemaWidget (Left schema)
    schemaWidget (Right (J.Object v)) = vBox $ elemWidget <$> (AK.toList v)
      where
        elemWidget (i, el) =
          (withAttr toolManagerSchemaKeyA $ txt $ AK.toText i)
            <+> padLeft (Pad 1) (schemaWidget $ Right el)
    schemaWidget (Right (J.Array v)) = vBox $ elemWidget <$> zip [0 ..] (toList v)
      where
        elemWidget (i, el) =
          (withAttr toolManagerSchemaKeyA $ txt $ mconcat ["[", T.show (i :: Int), "]"])
            <+> padLeft (Pad 1) (schemaWidget $ Right el)
    schemaWidget (Right (J.String t)) = txtWrap t
    schemaWidget (Right (J.Number n)) = txt $ T.show n
    schemaWidget (Right (J.Bool b)) = txt $ T.show b
    schemaWidget (Right J.Null) = txt "null"

drawState :: Bool -> ToolState -> Widget n
drawState selected (enabled, name, Tools.Tool {description = Tools.Description {description}}) =
  applyWhen selected (withDefAttr toolManagerSelectedA) $
    vBox
      [ withAttr toolManagerToolTitleA $ txt (check <> name),
        txtWrap description
      ]
  where
    check = if enabled then "● " else "○ "

helpText :: T.Text
helpText = " | k/j Move | <Space> Toggle"
