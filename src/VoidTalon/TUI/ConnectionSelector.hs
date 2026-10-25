{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI.ConnectionSelector (Selector (), newSelector, handleEvent, draw) where

import Brick
import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Table (renderTable, table)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.TH (makeLensesFor)
import VoidTalon.Config (ConnectionConfig (..))
import VoidTalon.TUI.FuzzyList
import VoidTalon.TUI.Types (Event (EvClosePopup, EvConnectionChange), Name (NConnectionSelector, NConnectionSelectorVP), PopupContext (..))
import qualified VoidTalon.Util as Util

data Selector = Selector
  { connections :: FuzzyList Name ConnectionConfig
  }

makeLensesFor
  [ ("connections", "selectorConnectionsL")
  ]
  ''Selector

newSelector :: [ConnectionConfig] -> Selector
newSelector connections =
  Selector
    { connections =
        newFuzzyList NConnectionSelector NConnectionSelectorVP renderEntry connections
    }
  where
    renderEntry ConnectionConfig {name} = name

handleEvent :: PopupContext -> BrickEvent Name e -> EventM Name Selector ()
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey V.KEsc [])) =
  liftIO $ Util.blockWriteBufferedBChan evchan EvClosePopup
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey V.KEnter [])) = do
  cons <- gets (.connections)
  case fuzzyListSelectedElement cons of
    Just c -> liftIO $ Util.blockWriteBufferedBChan evchan $ EvConnectionChange c
    Nothing -> pure ()
handleEvent _ ev = zoom selectorConnectionsL $ handleFuzzyListEvent ev

draw :: Selector -> Widget Name
draw Selector {connections} = hBox [connectionList, vBorder, details]
  where
    connectionList = hLimitPercent 50 $ renderFuzzyList renderEntry connections
    renderEntry _ _ = txt
    details = case fuzzyListSelectedElement connections of
      Just ConnectionConfig {base_url, headers, defaultModel} ->
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
