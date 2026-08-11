{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI.ModelSelector
  ( Selector (active),
    newSelector,
    handleEvent,
    draw,
    onOpened,
    modelsReceived,
  )
where

import Brick
import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Center (center)
import Brick.Widgets.List (List, handleListEventVi, list, listSelectedElement, renderList)
import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (catch)
import Control.Exception.Base (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro (Traversal')
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLensesFor)
import Network.HTTP.Client (Manager)
import VoidTalon.Config (Config (..), ConnectionConfig)
import qualified VoidTalon.Net.Models as M
import VoidTalon.TUI.Types (Event (..), Name (NModelSelector), PopupContext (..))
import VoidTalon.Util (BufferedBChan)
import qualified VoidTalon.Util as Util

data AvailableModels
  = -- | We don't know the available models and haven't asked yet
    AMUnknown
  | -- | A request for available  models is ongoing
    AMInFlight ThreadId
  | -- | Models received from server
    AMModels (List Name M.ModelInfo)

availableModelsModelsL :: Traversal' AvailableModels (List Name M.ModelInfo)
availableModelsModelsL f = \case
  AMUnknown -> pure AMUnknown
  AMInFlight t -> pure (AMInFlight t)
  AMModels x -> AMModels <$> f x

data Selector = Selector
  { -- | Currently active model ID.  Isn't necessarily one that's reported by the server.
    active :: Maybe T.Text,
    available :: AvailableModels
  }

makeLensesFor
  [ ("active", "selectorActiveL"),
    ("available", "selectorAvailableL")
  ]
  ''Selector

newSelector :: Maybe T.Text -> Selector
newSelector active =
  Selector
    { active,
      available = AMUnknown
    }

handleEvent :: PopupContext -> BrickEvent Name e -> EventM Name Selector ()
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey V.KEsc [])) =
  liftIO $ Util.blockWriteBufferedBChan EvClosePopup evchan
handleEvent _ (VtyEvent (V.EvKey V.KEnter [])) = do
  avail <- gets (.available)
  case avail of
    AMModels ms -> case listSelectedElement ms of
      Just (_, M.ModelInfo {id = id'}) -> selectorActiveL .= Just id'
      Nothing -> pure ()
    _ -> pure ()
handleEvent PopupContext {config, httpMan, evchan} (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
  avail <- gets (.available)
  case avail of
    AMInFlight _ -> pure ()
    _ -> startModelRequest config.connection httpMan evchan
handleEvent _ (VtyEvent ev) =
  zoom (selectorAvailableL . availableModelsModelsL) $
    handleListEventVi (const $ pure ()) ev
handleEvent _ _ = pure ()

draw :: Selector -> Widget Name
draw sel = hBox [modelList, vBorder, details]
  where
    modelList = hLimitPercent 50 $ case sel.available of
      AMModels ms -> renderList renderModelEntry True ms
      AMUnknown -> center $ txt "Models Unknown"
      AMInFlight _ -> center $ txt "Models Loading..."
    renderModelEntry _ M.ModelInfo {id = id', name} =
      let dpy = fromMaybe id' name
       in txt dpy
    details = case sel.available of
      AMModels ms -> case listSelectedElement ms of
        Just (_, el) ->
          vBox $
            catMaybes
              [ padBottom (Pad 1) . txtWrap <$> el.description,
                infoW "Context Length" . T.show <$> el.contextLength,
                infoW "Pricing" . pricingStr <$> el.pricing
              ]
        Nothing -> emptyWidget
      _ -> emptyWidget
    infoW name val = (padRight (Pad 1) $ txt name) <+> txtWrap val
    pricingStr M.Pricing {completion, prompt} =
      mconcat
        [ prompt,
          " $/t in ",
          completion,
          " $/t out"
        ]

startModelRequest :: ConnectionConfig -> Manager -> BufferedBChan Event -> EventM Name Selector ()
startModelRequest con man chan = do
  tid <- liftIO $ forkIO request
  selectorAvailableL .= AMInFlight tid
  where
    request = do
      models <- catch (M.list con man) exnHandler
      Util.writeBufferedBChan (EvModelList models) chan
      pure ()

    exnHandler :: SomeException -> IO [M.ModelInfo]
    exnHandler e = do
      -- TODO: log error properly
      putStrLn $ show e
      pure []

onOpened :: ConnectionConfig -> Manager -> BufferedBChan Event -> EventM Name Selector ()
onOpened con man chan = do
  avail <- gets (.available)
  case avail of
    AMUnknown -> startModelRequest con man chan
    _ -> pure ()

-- | Invoked from TUI when we get an EvModelList
modelsReceived :: [M.ModelInfo] -> EventM Name Selector ()
modelsReceived ms = (selectorAvailableL .=) . AMModels $ list NModelSelector (Vec.fromList ms) 1
