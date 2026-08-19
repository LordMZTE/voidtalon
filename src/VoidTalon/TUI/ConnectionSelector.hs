{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI.ConnectionSelector (Selector (), newSelector, handleEvent, draw) where

import Brick
import Brick.Widgets.Border (vBorder)
import Brick.Widgets.List
import Brick.Widgets.Table (renderTable, table)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro.TH (makeLensesFor)
import VoidTalon.Config (ConnectionConfig (..))
import VoidTalon.TUI.Types (Event (EvClosePopup, EvConnectionChange), Name (NConnectionSelector), PopupContext (..))
import qualified VoidTalon.Util as Util

data Selector = Selector
  { connections :: List Name ConnectionConfig
  }

makeLensesFor
  [ ("connections", "selectorConnectionsL")
  ]
  ''Selector

newSelector :: Vec.Vector ConnectionConfig -> Selector
newSelector connections = Selector {connections = list NConnectionSelector connections 1}

handleEvent :: PopupContext -> BrickEvent Name e -> EventM Name Selector ()
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey V.KEsc [])) =
  liftIO $ Util.blockWriteBufferedBChan evchan EvClosePopup
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey V.KEnter [])) = do
  cons <- gets (.connections)
  case listSelectedElement cons of
    Just (_, c) -> liftIO $ Util.blockWriteBufferedBChan evchan $ EvConnectionChange c
    Nothing -> pure ()
handleEvent _ (VtyEvent ev) = zoom selectorConnectionsL $ handleListEventVi (const $ pure ()) ev
handleEvent _ _ = pure ()

draw :: Selector -> Widget Name
draw Selector {connections} = hBox [connectionList, vBorder, details]
  where
    connectionList = hLimitPercent 50 $ renderList renderEntry True connections
    renderEntry _ ConnectionConfig {name} = txt name
    details = case listSelectedElement connections of
      Just (_, ConnectionConfig {base_url, headers, defaultModel}) ->
        let headerRow (k, v) =
              [ hLimitPercent 50 $ txt k,
                hLimitPercent 50 $ txt v
              ]
            headerTable =
              if Map.null headers
                then Nothing
                else
                  Just
                    . renderTable
                    $ table (headerRow <$> Map.toAscList headers)
         in vBox $
              [infoW "Base URL" (T.show base_url)]
                ++ maybeToList headerTable
                ++ (maybeToList $ infoW "Default Model" <$> defaultModel)
      Nothing -> emptyWidget
    infoW name val = (padRight (Pad 1) $ txt name) <+> txtWrap val
