{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI.ReasoningEffort
  ( Selector (),
    newSelector,
    draw,
    handleEvent,
    currentEffort,
  )
where

import Brick
import Brick.Widgets.Edit hiding (editor)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import qualified Graphics.Vty as V
import Lens.Micro.TH (makeLensesFor)
import qualified VoidTalon.Net.Completions as C
import qualified VoidTalon.Net.Models as M
import VoidTalon.TUI.Types (Event (..), Name (NReasoningEffort), PopupContext (..))
import qualified VoidTalon.Util as Util

data Selector = Selector
  { editor :: Editor T.Text Name
  }

makeLensesFor [("editor", "selectorEditorL")] ''Selector

newSelector :: Selector
newSelector =
  Selector
    { editor = editorText NReasoningEffort (Just 1) T.empty
    }

draw :: Maybe M.ModelInfo -> Selector -> Widget Name
draw info Selector {editor} =
  vBox
    [ txtWrap headerText,
      txt " ", -- spacer
      txtWrap infoText,
      txt "> " <+> renderEditor (txt . T.intercalate "\n") True editor
    ]
  where
    headerText =
      mconcat
        [ "Leave empty to send no reasoning effort, enter a number to set a limit of tokens",
          " for reasoning (only supported on llama.cpp, may cause breakage on other connections),",
          " enter a string to send that reasoning effort to the server."
        ]
    infoText = case info >>= (.supportedReasoningEfforts) of
      Just [] ->
        "Connection reports an empty list of supported reasoning efforts for the current model."
      Just effs ->
        "Connection reports these supported reasoning efforts for the current model:\n"
          <> T.intercalate ", " effs
      Nothing -> "Connection didn't report any reasoning efforts for the current model."

handleEvent :: PopupContext -> BrickEvent Name e -> EventM Name Selector ()
handleEvent PopupContext {evchan} (VtyEvent (V.EvKey k []))
  | k == V.KEsc || k == V.KEnter = liftIO $ Util.blockWriteBufferedBChan evchan EvClosePopup
handleEvent _ ev = zoom selectorEditorL $ handleEditorEvent ev

-- | Gets the current reasoning effort as we want to pass it to the API
currentEffort :: Selector -> C.ReasoningEffort
currentEffort Selector {editor} =
  if T.null s
    then C.REUnspecified
    else case decimal s of
      Right (n, rest) | T.null rest -> C.RELlamaCppTokens n
      _ -> C.REStr s
  where
    s = T.intercalate "\n" $ getEditContents editor
