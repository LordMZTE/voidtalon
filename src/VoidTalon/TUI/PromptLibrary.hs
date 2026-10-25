{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module VoidTalon.TUI.PromptLibrary
  ( Library (),
    newLibrary,
    draw,
    handleEvent,
    onOpened,
    runPromptInTUI,
    execInfoForContext,
  )
where

import Brick
import Brick.Widgets.Center (center)
import Control.Exception (try)
import Control.Exception.Base (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
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
import VoidTalon.TUI.FuzzyList

data Prompt = LocalPrompt String | MCPPrompt (T.Text, T.Text)

drawPrompt :: Prompt -> Widget n
drawPrompt (LocalPrompt s) = str s
drawPrompt (MCPPrompt (srv, _)) = withAttr toolManagerToolTitleA . txt $ "[MCP Instr.] " <> srv

type PromptList = FuzzyList Name Prompt

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
    | null $ fuzzyListEntries ps -> noPromptsWidget
    | otherwise -> renderFuzzyList (\_ p _ -> drawPrompt p) ps
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
  ctx@PopupContext {evchan}
  (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    case st.prompts >>= fuzzyListSelectedElement of
      Just (LocalPrompt p) -> do
        let execInfo = execInfoForContext ctx
        res <- readPrompt st.configDir p execInfo
        case res :: Either SomeException T.Text of
          Left e -> liftIO $ Util.blockWriteBufferedBChan evchan $ EvError $ show e
          Right content ->
            liftIO $ sendContents content
      Just (MCPPrompt (_, content)) -> liftIO $ sendContents content
      Nothing -> pure ()
    where
      readPrompt configDir name =
        ((liftIO $ PL.inspectPrompt configDir name) >>=) . runPromptInTUI
      sendContents :: T.Text -> IO ()
      sendContents content =
        Util.blockWriteBufferedBChanAllRev
          evchan
          [EvClosePopup, EvFillPromptEditor $ T.lines content]
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) = loadPrompts evchan
handleEvent _ ev =
  zoom (libraryPromptsL . _Just) $
    handleFuzzyListEvent ev

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
            newFuzzyList
              NPromptLibrary
              NPromptLibraryVP
              promptText
              ( (MCPPrompt <$> mcpPrompts) ++ (LocalPrompt <$> ps))
      libraryPromptsL .= Just ps'
      pure ()
  where
    promptText (MCPPrompt (n, _)) = n
    promptText (LocalPrompt t) = T.pack t

onOpened :: Util.BufferedBChan Event -> EventM Name Library ()
onOpened evchan =
  gets (.prompts) >>= \case
    Just _ -> pure ()
    Nothing -> loadPrompts evchan

runPromptInTUI ::
  (Ord n) => PL.ExecInfo -> PL.PromptInfo -> EventM n s (Either SomeException T.Text)
runPromptInTUI execInfo (info@(_, exec)) = go . try $ PL.evalPrompt info execInfo
  where
    go = if exec then suspendAndResume' else liftIO

execInfoForContext :: PopupContext -> PL.ExecInfo
execInfoForContext
  PopupContext
    { model,
      connection,
      reasoning
    } = PL.ExecInfo {model, connection = connection.name, reasoning}
