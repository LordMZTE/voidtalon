{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module VoidTalon.TUI.Markdown
  ( inlineWidget,
    docWidget,
    markdownWidget,
    wordWrap,
    highlightedCode,
  )
where

import Brick
import qualified Brick.BorderMap as BorderMap
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Table (ColumnAlignment (..), renderTable, setColAlignment, table)
import Commonmark (ListType (BulletList, OrderedList), commonmarkWith)
import Commonmark.Extensions (ColAlignment (..))
import Data.Bits ((.|.))
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (groupBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Text as T
import Graphics.Vty
  ( Attr (..),
    Color (Color240, RGBColor),
    Image,
    MaybeDefault (Default, KeepCurrent),
    blue,
    bold,
    currentAttr,
    defAttr,
    green,
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
import Lens.Micro
import qualified Skylighting as SL
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

mathAttr :: Attr -> Attr
mathAttr a = a {attrForeColor = SetTo green}

inlineWidget :: M.Inline -> Widget a
inlineWidget inl = lineWrapWidget $ do
  attr <- (^. attrL) <$> getContext
  pure $ splitLines $ segs attr inl
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
    segs a (InlineMath m) = math a m
    segs a (InlineMathBlock m) = math a m

    math a m = pure $ Just $ text' (mathAttr a) m

    splitLines :: [Maybe a] -> [[a]]
    splitLines = fmap catMaybes . groupBy (flip $ const . isJust)

-- | Given a list of lines of Images, returns a widget that renders these lines with wrapping.
lineWrapWidget :: RenderM n [[Image]] -> Widget n
lineWrapWidget lineImages = Widget Greedy Fixed $ do
  imgs <- lineImages
  c <- getContext
  let img = vertCat $ lineImage <$> (concatMap (wordWrap imageWidth c.availWidth) imgs)
  pure $ Result img [] [] [] BorderMap.empty
  where
    -- Concats multiple images to an image, but doesn't turn the empty line to an empty image, but
    -- rather a text image with no characters in order to create an empty line.
    lineImage :: [Image] -> Image
    lineImage [] = text' currentAttr ""
    lineImage imgs = horizCat imgs

-- | Like words, but keeps spaces
splitSpace :: T.Text -> [T.Text]
splitSpace = T.groupBy group
  where
    group _ ' ' = False
    group _ _ = True

docWidget :: M.Doc -> Widget a
docWidget = vBox . widgs
  where
    widgs DocEmpty = []
    widgs (DocConcat a b) = widgs a ++ widgs b
    -- Center math blocks
    widgs (DocPar mb@(InlineMathBlock _)) = [hCenter $ inlineWidget mb]
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
      [ modifyDefAttr
          (flip withStyle underline)
          ( str (take depth (repeat '#') <> " ")
              <+> inlineWidget t
          )
      ]
    widgs (DocCodeBlock lang t) = pure $
      case SL.lookupSyntax lang SL.defaultSyntaxMap of
        Just syntax -> borderWithLabel (txt syntax.sName) $ highlightedCode syntax t
        Nothing -> borderWithLabel (txt lang) $ txtWrap t -- Unknown language
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
    widgs (DocTable _ []) = []
    widgs (DocTable align rows@(hdr : _)) = pure $ Widget Greedy Fixed $ do
      c <- getContext
      let nhdrs = length hdr
      -- Width that's available to fill for each cell if split evenly
      let availWidth = ((c.availWidth - 1) `div` nhdrs) - 1
      let tab = table $ (hLimit availWidth . inlineWidget <$>) <$> rows
      let tab' = foldr ($) tab $ uncurry setColAlignment <$> zip (alignment <$> align) [0 ..]
      render $ renderTable tab'
      where
        alignment :: ColAlignment -> ColumnAlignment
        alignment LeftAlignedCol = AlignLeft
        alignment DefaultAlignedCol = AlignLeft
        alignment RightAlignedCol = AlignRight
        alignment CenterAlignedCol = AlignCenter

markdownWidget :: String -> T.Text -> Widget a
markdownWidget docname input = case runIdentity $ commonmarkWith M.spec docname input of
  Left err -> withAttr TT.warningA (strWrap $ show err) <=> txtWrap input
  Right doc -> docWidget doc

-- | Wraps words.  Given a function that, for some "word" type `a` returns the length of that word,
-- the line length and the words, returns a list of lines.
-- If a non-empty list is given, we always return at least one element per line.  This might lead to
-- the line overflowing, but we have no way of splitting words, so that's just gonna be how it is.
wordWrap :: (Num n, Ord n) => (a -> n) -> n -> [a] -> [[a]]
wordWrap _ _ [] = [[]] -- Avoid trimming empty lines
wordWrap f len ws = inner ws
  where
    inner ws' = case linePair 0 ws' of
      -- If we got an empty list, we had one word that was too long for the line.
      ([], w : ws'') -> [w] : (inner ws'')
      (l, []) -> [l]
      (l, ls) -> l : (inner ls)
    linePair _ [] = ([], [])
    linePair n xs@(x : xs') =
      let newLen = n + f x
       in if newLen > len
            then ([], xs)
            else let (ys, zs) = linePair newLen xs' in (x : ys, zs)

highlightedCode ::
  SL.Syntax ->
  T.Text ->
  Widget a
highlightedCode syntax t = case tokResult of
  Left _ -> txtWrap t -- Error, fallback to unhighlighted text
  Right sourceLines -> lineWrapWidget $ pure $ (tokImage <$>) <$> sourceLines
  where
    tokCfg =
      SL.TokenizerConfig
        { SL.syntaxMap = SL.defaultSyntaxMap, -- no idea what this is even needed for
          SL.traceOutput = False
        }
    tokResult = SL.tokenize tokCfg syntax t

    tokImage (ttyp, text) = text' (ttypeAttr ttyp) text

    ttypeAttr ttyp =
      fromMaybe defAttr $ slStyleToVTY <$> catppuccinMocha.tokenStyles Map.!? ttyp

    slStyleToVTY :: SL.TokenStyle -> Attr
    slStyleToVTY tstyle =
      Attr
        { attrStyle =
            SetTo $
              (if tstyle.tokenBold then bold else 0)
                .|. (if tstyle.tokenItalic then italic else 0)
                .|. (if tstyle.tokenUnderline then underline else 0),
          attrForeColor = slColorToVTY tstyle.tokenColor,
          attrBackColor = slColorToVTY tstyle.tokenBackground,
          attrURL = Default
        }
    slColorToVTY Nothing = KeepCurrent
    slColorToVTY (Just (SL.RGB r g b)) = SetTo $ RGBColor r g b

-- | This is created by using SL.parseTheme to parse and then show this theme:
-- https://github.com/catppuccin/ksyntaxhighlighting/blob/main/themes/mocha.theme
catppuccinMocha :: SL.Style
catppuccinMocha =
  SL.Style
    { SL.tokenStyles =
        Map.fromList
          [ (SL.KeywordTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 203 166 247)}),
            (SL.DataTypeTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 203 166 247)}),
            (SL.DecValTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 250 179 135)}),
            (SL.BaseNTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 250 179 135)}),
            (SL.FloatTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 250 179 135)}),
            (SL.ConstantTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 250 179 135)}),
            (SL.CharTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 245 194 231)}),
            (SL.SpecialCharTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 245 194 231)}),
            (SL.StringTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 166 227 161)}),
            (SL.VerbatimStringTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 243 139 168)}),
            (SL.SpecialStringTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 243 139 168)}),
            (SL.ImportTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 166 227 161)}),
            (SL.CommentTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 108 112 134), SL.tokenItalic = True}),
            (SL.DocumentationTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 243 139 168)}),
            (SL.AnnotationTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 249 226 175)}),
            (SL.CommentVarTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 108 112 134), SL.tokenItalic = True}),
            (SL.OtherTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 250 179 135)}),
            (SL.FunctionTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 137 180 250)}),
            (SL.VariableTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 245 194 231)}),
            (SL.ControlFlowTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 203 166 247)}),
            (SL.OperatorTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 137 220 235)}),
            (SL.BuiltInTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 243 139 168)}),
            (SL.ExtensionTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 137 180 250)}),
            (SL.PreprocessorTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 245 194 231)}),
            (SL.AttributeTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 180 190 254)}),
            (SL.RegionMarkerTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 108 112 134), SL.tokenItalic = True}),
            (SL.InformationTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 250 179 135)}),
            (SL.WarningTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 250 179 135)}),
            (SL.AlertTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 243 139 168), SL.tokenBold = True}),
            (SL.ErrorTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 243 139 168), SL.tokenUnderline = True}),
            (SL.NormalTok, SL.defStyle {SL.tokenColor = Just (SL.RGB 205 214 244)})
          ],
      SL.defaultColor = Just (SL.RGB 205 214 244),
      SL.backgroundColor = Nothing,
      SL.lineNumberColor = Nothing,
      SL.lineNumberBackgroundColor = Nothing
    }
