{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module VoidTalon.TUI.PromptLibrary (Library (), newLibrary, draw, handleEvent, onOpened) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.List
import Control.Exception (try)
import Control.Exception.Base (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro (_Just)
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLensesFor)
import System.FilePath ((</>))
import VoidTalon.Config (ConnectionConfig (..))
import VoidTalon.Net.MCP (Server (..), ServerInfo (..))
import qualified VoidTalon.Net.MCP as MCP
import qualified VoidTalon.PromptLibrary as PL
import VoidTalon.TUI.Types (Event (..), Name (..), PopupContext (..), toolManagerToolTitleA)
import qualified VoidTalon.Util as Util

data Prompt = LocalPrompt String | MCPPrompt (T.Text, T.Text)

drawPrompt :: Prompt -> Widget n
drawPrompt (LocalPrompt s) = str s
drawPrompt (MCPPrompt (srv, _)) = withAttr toolManagerToolTitleA . txt $ "[MCP Instr.] " <> srv

type PromptList = List Name Prompt

data Library = Library
  { configDir :: FilePath,
    mcpPrompts :: [(T.Text, T.Text)],
    prompts :: Maybe PromptList
  }

makeLensesFor [("prompts", "libraryPromptsL")] ''Library

newLibrary ::
  -- | Config base directory
  FilePath ->
  [MCP.Server] ->
  Library
newLibrary configDir mcps =
  Library
    { configDir,
      mcpPrompts =
        mapMaybe
          ( \m ->
              (fromMaybe m.serverInfo.name m.serverInfo.title,)
                <$> m.instructions
          )
          mcps,
      prompts = Nothing
    }

draw :: Library -> Widget Name
draw Library {configDir, prompts} = case prompts of
  Just ps
    | Vec.null $ listElements ps -> noPromptsWidget
    | otherwise -> renderList (const drawPrompt) True ps
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
handleEvent
  PopupContext
    { evchan,
      model,
      connection,
      reasoning
    }
  (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    case st.prompts >>= listSelectedElement of
      Just (_, LocalPrompt p) -> do
        let execInfo = PL.ExecInfo {model, connection = connection.name, reasoning}
        res <- readPrompt st.configDir p execInfo
        case res :: Either SomeException T.Text of
          Left e -> liftIO $ Util.blockWriteBufferedBChan evchan $ EvError $ show e
          Right content ->
            liftIO $ sendContents content
      Just (_, MCPPrompt (_, content)) -> liftIO $ sendContents content
      Nothing -> pure ()
    where
      readPrompt ::
        (Ord n) =>
        FilePath ->
        String ->
        PL.ExecInfo ->
        EventM n s (Either SomeException T.Text)
      readPrompt configDir name execInfo = do
        info@(_, exec) <- liftIO $ PL.inspectPrompt configDir name
        let f = if exec then suspendAndResume' else liftIO
        f $ try $ PL.evalPrompt info execInfo
      sendContents :: T.Text -> IO ()
      sendContents content =
        Util.blockWriteBufferedBChanAllRev
          evchan
          [EvClosePopup, EvFillPromptEditor $ T.lines content]
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey (V.KChar 'r') [])) = loadPrompts evchan
handleEvent _ (VtyEvent ev) =
  zoom (libraryPromptsL . _Just) $
    handleListEventVi (const $ pure ()) ev
handleEvent _ _ = pure ()

loadPrompts :: Util.BufferedBChan Event -> EventM Name Library ()
loadPrompts evchan = do
  Library {configDir, mcpPrompts} <- get
  -- Theoretically, this could run in the background as it's disk IO, but I figured this was not
  -- worth the effort.
  res <- liftIO $ try $ PL.listPrompts configDir
  case res :: Either SomeException [String] of
    Left e -> liftIO $ Util.blockWriteBufferedBChan evchan $ EvError $ show e
    Right ps -> do
      let ps' =
            list
              NPromptLibrary
              ( Vec.fromList $
                  (MCPPrompt <$> mcpPrompts) ++ (LocalPrompt <$> ps)
              )
              1
      libraryPromptsL .= Just ps'
      pure ()

onOpened :: Util.BufferedBChan Event -> EventM Name Library ()
onOpened evchan =
  gets (.prompts) >>= \case
    Just _ -> pure ()
    Nothing -> loadPrompts evchan
