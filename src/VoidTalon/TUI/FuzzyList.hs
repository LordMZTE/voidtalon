{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.TUI.FuzzyList
  ( FuzzyList,
    newFuzzyList,
    handleFuzzyListEvent,
    renderFuzzyList,
    fuzzyListSelectedElement,
    fuzzyListEntries,
    fuzzyListEntriesL,
  )
where

import Brick hiding (cached)
import Brick.Widgets.Border
import Brick.Widgets.Edit hiding (editor)
import Brick.Widgets.List (listSelectedAttr)
import Data.Function (applyWhen)
import Data.List ((!?))
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLensesFor)
import Text.Fuzzily
import VoidTalon.Util (focusAdd, focusSub)
import Prelude hiding (filter)

data FuzzyList n e = FuzzyList
  { vpName :: n,
    editor :: Editor T.Text n,
    render :: e -> T.Text,
    entries :: [e],
    selected :: Int,
    -- | List of sorted entries as should be displayed in the TUI.  Updated when the user edits
    -- search.
    cached :: [Fuzzy e T.Text]
  }

makeLensesFor
  [ ("editor", "fuzzyListEditorL"),
    ("selected", "fuzzyListSelectedL")
  ]
  ''FuzzyList

newFuzzyList :: n -> n -> (e -> T.Text) -> [e] -> FuzzyList n e
newFuzzyList editName vpName render entries =
  FuzzyList
    { vpName,
      editor = editorText editName (Just 1) T.empty,
      render,
      entries,
      selected = 0,
      cached = (\original -> Fuzzy {original, rendered = render original, score = 0}) <$> entries
    }

renderFuzzyList :: (Ord n, Show n) => (Bool -> e -> T.Text -> Widget n) -> FuzzyList n e -> Widget n
renderFuzzyList renderEntry FuzzyList {vpName, editor, selected, cached} =
  vBox [searchBar, hBorder, entriesWidget]
  where
    searchBar = txt "> " <+> renderEditor (txt . T.intercalate "\n") True editor
    entriesWidget = withVScrollBars OnLeft . viewport vpName Vertical $ vBox entries
    entries = uncurry entryWidget <$> zip cached [0 ..]
    entryWidget Fuzzy {original, rendered} idx =
      let isSelected = idx == selected
       in applyWhen isSelected (withDefAttr listSelectedAttr) $
            renderEntry isSelected original rendered

handleFuzzyListEvent :: (Eq n) => BrickEvent n ev -> EventM n (FuzzyList n e) ()
handleFuzzyListEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  total <- gets $ length . (.cached)
  fuzzyListSelectedL %= focusAdd total
handleFuzzyListEvent (VtyEvent (V.EvKey (V.KBackTab) [])) = do
  total <- gets $ length . (.cached)
  fuzzyListSelectedL %= focusSub total
handleFuzzyListEvent ev = do
  -- Delegate event to editor
  zoom fuzzyListEditorL $ handleEditorEvent ev
  -- Update cached entries since the search query might've changed.
  modify recomputeCached

fuzzyListSelectedElement :: FuzzyList n e -> Maybe e
fuzzyListSelectedElement FuzzyList {cached, selected} = (.original) <$> cached !? selected

fuzzyListEntries :: FuzzyList n e -> [e]
fuzzyListEntries = (.entries)

fuzzyListEntriesL :: Lens' (FuzzyList n e) [e]
fuzzyListEntriesL f l =
  (\entries -> recomputeCached l {entries})
    <$> f l.entries

recomputeCached :: FuzzyList n e -> FuzzyList n e
recomputeCached l@FuzzyList {render, editor, entries, selected} =
  l
    { cached,
      selected = clamp 0 (length cached - 1) selected
    }
  where
    cached =
      filter
        IgnoreCase
        (T.empty, T.empty)
        render
        (T.intercalate "\n" $ getEditContents editor)
        entries
