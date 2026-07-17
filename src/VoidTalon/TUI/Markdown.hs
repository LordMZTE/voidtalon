{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.TUI.Markdown (inlineWidget, docWidget, markdownWidget, wordWrap) where

import Brick
import qualified Brick.BorderMap as BorderMap
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Commonmark (ListType (BulletList, OrderedList))
import Commonmark.Parser (commonmark)
import Data.List (groupBy)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import Graphics.Vty
  ( Attr (attrBackColor, attrForeColor, attrURL),
    Color (Color240),
    blue,
    bold,
    currentAttr,
    horizCat,
    imageHeight,
    imageWidth,
    italic,
    text',
    underline,
    vertCat,
    withStyle,
  )
import Graphics.Vty.Attributes (MaybeDefault (SetTo))
import VoidTalon.Markdown as M
import qualified VoidTalon.TUI.Types as TT

codeAttr :: Attr -> Attr
codeAttr a = a {attrBackColor = SetTo $ Color240 $ 241 - 16}

emphAttr :: Attr -> Attr
emphAttr = flip withStyle italic

strongAttr :: Attr -> Attr
strongAttr = flip withStyle bold

linkAttr :: T.Text -> Attr -> Attr
linkAttr url a =
  withStyle
    ( a
        { attrURL = SetTo url,
          attrForeColor = SetTo blue
        }
    )
    underline

inlineWidgetWith :: Attr -> M.Inline -> Widget a
inlineWidgetWith attr inl = Widget Greedy Fixed $ do
  c <- getContext
  let img =
        vertCat $
          horizCat
            <$> (concatMap (wrap c.availWidth) $ splitLines $ segs attr inl)
  pure $ Result img [] [] [] BorderMap.empty
  where
    -- We use Just to mean a span of text, and Nothing to mean a line break.
    segs _ InlineEmpty = []
    segs a (InlineConcat l r) = segs a l ++ segs a r
    segs _ (InlineBreak) = [Nothing]
    segs a (InlineText t) = Just . text' a <$> splitSpace t
    segs a (InlineCode t) = Just . text' (codeAttr a) <$> splitSpace t
    segs a (InlineEmph t) = segs (emphAttr a) t
    segs a (InlineStrong t) = segs (strongAttr a) t
    segs a (InlineLink _ dest _ t) = segs (linkAttr dest a) t

    -- Small wrapper around wordWrap that prevents our code from stripping empty lines.
    wrap _ [] = [[]]
    wrap width ws = wordWrap imageWidth width ws

    splitLines :: [Maybe a] -> [[a]]
    splitLines = fmap catMaybes . groupBy (flip $ const . isJust)

-- | Like words, but keeps spaces
splitSpace :: T.Text -> [T.Text]
splitSpace = T.groupBy group
  where
    group _ ' ' = False
    group _ _ = True

inlineWidget :: M.Inline -> Widget a
inlineWidget = inlineWidgetWith currentAttr

docWidget :: M.Doc -> Widget a
docWidget = vBox . widgs
  where
    widgs DocEmpty = []
    widgs (DocConcat a b) = widgs a ++ widgs b
    widgs (DocPar t) = [inlineWidget t]
    widgs DocRule = [hBorder]
    widgs (DocList t xs) =
      uncurry (<+>) <$> rows
      where
        bullet = txt "• "
        rows :: [(Widget a, Widget a)]
        rows = case t of
          BulletList _ -> (\d -> (bullet, docWidget d)) <$> xs
          OrderedList start _ _ ->
            (\(n, d) -> (str $ (show n) <> ". ", docWidget d))
              <$> zip [start ..] xs
    widgs (DocHeading depth t) =
      [ str (take depth (repeat '#') <> " ")
          <+> inlineWidgetWith (withStyle currentAttr underline) t
      ]
    widgs (DocCodeBlock lang t) = [borderWithLabel (txt lang) $ txtWrap t] -- TODO syntax hl
    widgs (DocQuote t) = pure $ Widget hInner vInner $ do
      c <- getContext
      resInner <-
        render $
          hLimit (c.availWidth - 2) $
            vLimit (c.availHeight) $
              inner
      let finalHeight = imageHeight resInner.image
      let border = hLimit 1 . vLimit finalHeight $ fill '┃'
      let spacer = hLimit 1 . vLimit finalHeight $ fill ' '
      render $ hBox [border, spacer, TT.bakedWidget resInner]
      where
        inner@(Widget hInner vInner _) = docWidget t

markdownWidget :: String -> T.Text -> Widget a
markdownWidget docname input = case commonmark docname input of
  Left err -> withAttr TT.warningA (strWrap $ show err) <=> txtWrap input
  Right doc -> docWidget doc

-- | Wraps words.  Given a function that, for some "word" type `a` returns the length of that word,
-- the line length and the words, returns a list of lines.
-- If a non-empty list is given, we always return at least one element per line.  This might lead to
-- the line overflowing, but we have no way of splitting words, so that's just gonna be how it is.
wordWrap :: (Num n, Ord n) => (a -> n) -> n -> [a] -> [[a]]
wordWrap f len ws =
  case linePair 0 ws of
    -- If we got an empty list, we had one word that was too long for the line.
    ([], w : ws') -> [w] : (wordWrap f len ws')
    (l, []) -> [l]
    (l, ls) -> l : (wordWrap f len ls)
  where
    linePair _ [] = ([], [])
    linePair n xs@(x : xs') =
      let newLen = n + f x
       in if newLen > len
            then ([], xs)
            else let (ys, zs) = linePair newLen xs' in (x : ys, zs)
