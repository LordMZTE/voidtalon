{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI.ToolManager
  ( Manager,
    newManager,
    activeTools,
    findTool,
    handleEvent,
    draw,
  )
where

import Brick
import Brick.Widgets.Border (vBorder)
import Brick.Widgets.List (listSelectedAttr)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (State, execState)
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AK
import Data.Foldable (toList)
import Data.Function (applyWhen)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLensesFor)
import VoidTalon.JSON (Schema (..), SchemaType)
import VoidTalon.TUI.Icons (circleEmpty, circleFilled, diamondEmpty, diamondFilled, foldClosed, foldOpen)
import qualified VoidTalon.TUI.Icons as Icons
import VoidTalon.TUI.Types
  ( Event (EvClosePopup),
    Name (NToolManagerEntry, NToolManagerVP),
    PopupContext (..),
    toolManagerSchemaKeyA,
    toolManagerSchemaTypeA,
    toolManagerToolGroupTitleA,
    toolManagerToolTitleA,
  )
import qualified VoidTalon.Tools as Tools
import qualified VoidTalon.Util as Util

data Manager = Manager
  { groups :: ![Tools.Group],
    selected :: Int
  }

makeLensesFor
  [ ("groups", "managerGroupsL"),
    ("selected", "managerSelectedL")
  ]
  ''Manager

allStates :: [Tools.Group] -> [Tools.State]
allStates = concatMap (.states)

applyToSelectedM ::
  (Monad m) =>
  -- | Selected index
  Int ->
  -- | Function to apply when a group is selected
  (Tools.Group -> m Tools.Group) ->
  -- | Function to apply when a single tool is selected
  (Tools.State -> m Tools.State) ->
  [Tools.Group] ->
  m [Tools.Group]
applyToSelectedM _ _ _ groups@[] = pure groups
applyToSelectedM 0 mapGroup _ (group : groups) = (: groups) <$> mapGroup group
applyToSelectedM idx mapGroup mapTool (group@Tools.Group {opened, states} : groups) =
  case opened of
    False -> (group :) <$> applyToSelectedM (idx - 1) mapGroup mapTool groups
    True
      | idx > nTools ->
          (group :)
            <$> applyToSelectedM
              (toolIdx - nTools)
              mapGroup
              mapTool
              groups
      | otherwise -> (: groups) <$> (mapMOf (Tools.groupStatesL . ix toolIdx) mapTool group)
  where
    nTools = length states
    -- We subtract one because we also got the group header in front
    toolIdx = idx - 1

applyToSelected ::
  Int ->
  (Tools.Group -> Tools.Group) ->
  (Tools.State -> Tools.State) ->
  [Tools.Group] ->
  [Tools.Group]
applyToSelected idx mapGroup mapTool groups =
  runIdentity $ applyToSelectedM idx (pure . mapGroup) (pure . mapTool) groups

getSelected :: Int -> [Tools.Group] -> Maybe (Either Tools.Group Tools.State)
getSelected idx groups =
  execState
    (applyToSelectedM idx (storeCurrent Left) (storeCurrent Right) groups)
    Nothing
  where
    storeCurrent ::
      (a -> Either Tools.Group Tools.State) -> a -> State (Maybe (Either Tools.Group Tools.State)) a
    storeCurrent mk x = (put . Just $ mk x) >> pure x

listEntryCount :: [Tools.Group] -> Int
listEntryCount = foldl' (\a g -> a + groupEntryCount g) 0
  where
    groupEntryCount :: Tools.Group -> Int
    groupEntryCount Tools.Group {opened, states} = if opened then length states + 1 else 1

-- | Create a new manager that knows about the given tools
newManager :: [Tools.Group] -> Manager
newManager groups =
  Manager
    { groups,
      selected = 0
    }

-- | For each active tool, gets that tool's name and description.  This can then be used to create a
-- completions context directly.
activeTools :: Manager -> [(T.Text, Tools.Description)]
activeTools Manager {groups} =
  (\Tools.State {name, tool} -> (name, tool.description))
    <$> filter (.enabled) (allStates groups)

-- | Searches the currently active tools for one of the given name.
findTool :: Manager -> T.Text -> Maybe Tools.State
findTool Manager {groups} n =
  find (\Tools.State {enabled, name} -> enabled && n == name) (allStates groups)

handleEvent :: PopupContext -> BrickEvent Name e -> EventM Name Manager ()
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'j') [])) = do
  m <- get
  let sel = Util.focusAdd (listEntryCount m.groups) m.selected
  managerSelectedL .= sel
  makeVisible $ NToolManagerEntry sel
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'k') [])) = do
  m <- get
  let sel = Util.focusSub (listEntryCount m.groups) m.selected
  managerSelectedL .= sel
  makeVisible $ NToolManagerEntry sel
handleEvent _ (VtyEvent (V.EvKey (V.KChar ' ') [])) = toggleSelectedProperty Tools.stateEnabledL
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'a') [])) = toggleSelectedProperty Tools.stateAutoconfirmL
handleEvent _ (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  sel <- gets (.selected)
  managerGroupsL %= applyToSelected sel foldGroup id
  where
    foldGroup = Tools.groupOpenedL %~ not
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey V.KEsc [])) =
  liftIO $ Util.blockWriteBufferedBChan evchan EvClosePopup
handleEvent _ _ = pure ()

draw :: Manager -> Widget Name
draw Manager {groups, selected} = hBox [list, vBorder, schemaView]
  where
    list =
      withVScrollBars OnLeft
        . viewport NToolManagerVP Vertical
        . vBox
        $ ( \(e, i) ->
              reportExtent (NToolManagerEntry i) $
                applyWhen (i == selected) (withDefAttr listSelectedAttr) e
          )
          <$> zip entryWidgets [0 ..]

    entryWidgets :: [Widget Name]
    entryWidgets =
      let groupWidgets g@Tools.Group {opened, states} =
            drawTitle g : if opened then drawState <$> states else []
       in concatMap groupWidgets groups

    schemaView = case getSelected selected groups of
      Just (Left Tools.Group {description}) -> txtWrap $ fromMaybe "<no description>" description
      Just (Right Tools.State {tool = Tools.Tool {description}}) -> schemaWidget description.schema
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

drawTitle :: Tools.Group -> Widget n
drawTitle Tools.Group {opened, name, states} =
  withAttr toolManagerToolGroupTitleA . txt $
    mconcat [arrow, " ", enabledIcon, " ", autoconfirmIcon, " ", name]
  where
    arrow = T.singleton $ if opened then foldOpen else foldClosed
    enabledIcon = T.singleton $ case Util.classifyCount (.enabled) states of
      Util.CCSome -> Icons.circleHalf
      Util.CCAll -> Icons.circleFilled
      Util.CCNone -> Icons.circleEmpty
    autoconfirmIcon = T.singleton $ case Util.classifyCount (.autoconfirm) states of
      Util.CCSome -> Icons.diamondHalf
      Util.CCAll -> Icons.diamondFilled
      Util.CCNone -> Icons.diamondEmpty

drawState :: Tools.State -> Widget n
drawState
  Tools.State
    { enabled,
      autoconfirm,
      name,
      tool = Tools.Tool {description = Tools.Description {description}}
    } =
    txt "  "
      <+> vBox
        [ withAttr toolManagerToolTitleA . txt $
            mconcat [enabledIcon, " ", autoconfirmIcon, " ", name],
          txtWrap description
        ]
    where
      enabledIcon = T.singleton $ if enabled then circleFilled else circleEmpty
      autoconfirmIcon = T.singleton $ if autoconfirm then diamondFilled else diamondEmpty

toggleSelectedProperty :: Lens' Tools.State Bool -> EventM Name Manager ()
toggleSelectedProperty l = do
  sel <- gets (.selected)
  managerGroupsL %= applyToSelected sel toggleGroup toggleTool
  where
    toggleGroup :: Tools.Group -> Tools.Group
    toggleGroup group@Tools.Group {states} =
      -- When all tools are enabled, disable all, otherwise enable all.
      let allEnabled = all (^. l) group.states
          states' = states & each . l .~ not allEnabled
       in group {Tools.states = states'}
    toggleTool :: Tools.State -> Tools.State
    toggleTool = l %~ not
