{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI.PromptLibrary (Library (), newLibrary, draw, handleEvent, onOpened) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.List
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro (_Just)
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLensesFor)
import System.FilePath ((</>))
import qualified VoidTalon.PromptLibrary as PL
import VoidTalon.TUI.Types (Event (..), Name (..), PopupContext (..))
import qualified VoidTalon.Util as Util

type PromptList = List Name String

data Library = Library
  { configDir :: FilePath,
    prompts :: Maybe PromptList
  }

makeLensesFor [("prompts", "libraryPromptsL")] ''Library

newLibrary ::
  -- | Config base directory
  FilePath ->
  Library
newLibrary configDir = Library {configDir, prompts = Nothing}

draw :: Library -> Widget Name
draw Library {configDir, prompts} = case prompts of
  Just ps -> if Vec.null $ listElements ps then noPromptsWidget else renderList (const str) True ps
  Nothing -> unknownWidget
  where
    noPromptsWidget =
      center $
        (txt "No prompt templates in directory")
          <=> str (configDir </> PL.promptDirname)
    unknownWidget = center $ txt "Prompt templates unknown"

handleEvent :: PopupContext -> BrickEvent Name e -> EventM Name Library ()
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey V.KEsc [])) =
  liftIO $ Util.blockWriteBufferedBChan evchan EvClosePopup
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  case st.prompts of
    Just ps -> case listSelectedElement ps of
      Just (_, p) -> do
        res <- liftIO $ try $ PL.readPrompt st.configDir p
        case res :: Either SomeException T.Text of
          Left e -> liftIO $ Util.blockWriteBufferedBChan evchan $ EvError $ show e
          Right content ->
            liftIO $
              Util.blockWriteBufferedBChanAllRev
                evchan
                [EvClosePopup, EvFillPromptEditor $ T.lines content]
      Nothing -> pure ()
    Nothing -> pure ()
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey (V.KChar 'r') [])) = loadPrompts evchan
handleEvent _ (VtyEvent ev) =
  zoom (libraryPromptsL . _Just) $
    handleListEventVi (const $ pure ()) ev
handleEvent _ _ = pure ()

loadPrompts :: Util.BufferedBChan Event -> EventM Name Library ()
loadPrompts evchan = do
  -- Theoretically, this could run in the background as it's disk IO, but I figured this was not
  -- worth the effort.
  configDir <- gets (.configDir)
  res <- liftIO $ try $ PL.listPrompts configDir
  case res :: Either SomeException [String] of
    Left e -> liftIO $ Util.blockWriteBufferedBChan evchan $ EvError $ show e
    Right ps -> do
      let ps' = list NPromptLibrary (Vec.fromList ps) 1
      libraryPromptsL .= Just ps'
      pure ()

onOpened :: Util.BufferedBChan Event -> EventM Name Library ()
onOpened evchan =
  gets (.prompts) >>= \case
    Just _ -> pure ()
    Nothing -> loadPrompts evchan
