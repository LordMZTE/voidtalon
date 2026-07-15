{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.TUI.Markdown (inlineWidget, docWidget, markdownWidget) where

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Table (columnBorders, renderTable, rowBorders, surroundingBorder, table)
import Commonmark (ListType (BulletList, OrderedList))
import Commonmark.Parser (commonmark)
import qualified Data.Text as T
import Graphics.Vty (Attr (attrBackColor, attrForeColor, attrURL), Color (Color240), blue, bold, currentAttr, imageHeight, italic, text', underline, withStyle)
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
inlineWidgetWith attr = raw . img attr
  where
    img _ InlineEmpty = mempty
    img a (InlineConcat l r) = img a l <> img a r
    img a (InlineText t) = text' a t
    img a (InlineCode t) = text' (codeAttr a) t
    img a (InlineEmph t) = img (emphAttr a) t
    img a (InlineStrong t) = img (strongAttr a) t
    img a (InlineLink _ dest _ t) = img (linkAttr dest a) t

inlineWidget :: M.Inline -> Widget a
inlineWidget = inlineWidgetWith currentAttr

docWidget :: M.Doc -> Widget a
docWidget DocEmpty = emptyWidget
docWidget cons@(DocConcat _ _) = vBox $ flattenDoc cons
  where
    flattenDoc (DocConcat a b) = flattenDoc a ++ flattenDoc b
    flattenDoc x = [docWidget x]
docWidget (DocPar t) = inlineWidget t
docWidget DocRule = hBorder
docWidget (DocList t xs) =
  renderTable
    . surroundingBorder False
    . rowBorders False
    . columnBorders False
    . table
    $ rows
  where
    bullet = txt "• "
    rows = case t of
      BulletList _ -> (\d -> [bullet, docWidget d]) <$> xs
      OrderedList start _ _ ->
        (\(n, d) -> [str $ (show n) <> ". ", docWidget d])
          <$> zip [start ..] xs
docWidget (DocHeading depth t) =
  str (take depth (repeat '#') <> " ")
    <+> inlineWidgetWith (withStyle currentAttr underline) t
docWidget (DocCodeBlock lang t) = borderWithLabel (txt lang) $ txtWrap t -- TODO syntax hl
docWidget (DocQuote t) = Widget hInner vInner $ do
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
