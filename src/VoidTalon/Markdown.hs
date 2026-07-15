{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Markdown (Inline (..), Doc (..)) where

import qualified Commonmark as C
import qualified Data.Text as T

data Inline
  = InlineEmpty
  | InlineConcat Inline Inline
  | InlineBreak
  | InlineText T.Text
  | InlineCode T.Text
  | InlineEmph Inline
  | InlineStrong Inline
  | InlineLink Bool T.Text T.Text Inline -- isImage, dest, title, desc
  deriving (Show)

instance Semigroup Inline where
  InlineEmpty <> x = x
  x <> InlineEmpty = x
  InlineText a <> InlineText b = InlineText $ a <> b
  InlineCode a <> InlineCode b = InlineCode $ a <> b
  InlineEmph a <> InlineEmph b = InlineEmph $ a <> b
  InlineStrong a <> InlineStrong b = InlineStrong $ a <> b
  InlineConcat a b <> c = InlineConcat a $ b <> c
  a <> InlineConcat b c = InlineConcat (a <> b) c
  a <> b = InlineConcat a b

instance Monoid Inline where
  mempty = InlineEmpty

instance C.Rangeable Inline where
  ranged _ = id

instance C.HasAttributes Inline where
  addAttributes _ = id

instance C.IsInline Inline where
  lineBreak = InlineBreak
  softBreak = C.lineBreak
  str = InlineText
  entity = C.str -- No idea what this is
  escapedChar = C.str . T.singleton
  emph = InlineEmph
  strong = InlineStrong
  link = InlineLink False
  image = InlineLink True
  code = InlineCode
  rawInline _ = C.code

-- | A markdown AST suited for rendering to our TUI later
data Doc
  = DocEmpty
  | DocConcat Doc Doc
  | DocPar Inline
  | DocRule
  | DocList C.ListType [Doc]
  | DocHeading Int Inline
  | DocCodeBlock T.Text T.Text
  | DocQuote Doc
  deriving (Show)

instance Semigroup Doc where
  DocEmpty <> x = x
  x <> DocEmpty = x
  DocConcat a b <> c = DocConcat a $ b <> c
  a <> DocConcat b c = DocConcat (a <> b) c
  a <> b = DocConcat a b

instance Monoid Doc where
  mempty = DocEmpty

instance C.Rangeable Doc where
  ranged _ = id

instance C.HasAttributes Doc where
  addAttributes _ = id

instance C.IsBlock Inline Doc where
  paragraph = DocPar
  plain = C.paragraph -- Difference is unclear
  thematicBreak = DocRule
  blockQuote = DocQuote
  codeBlock = DocCodeBlock
  heading = DocHeading
  rawBlock (C.Format fmt) = DocCodeBlock fmt
  referenceLinkDefinition label (dest, title) = DocPar $ InlineText $ str
    where
      str =
        mconcat $
          if T.null title
            then ["[", label, "](", dest, ")"]
            else ["[", label, "](", dest, " \"", title, "\"", ")"]
  list ltype _ = DocList ltype
