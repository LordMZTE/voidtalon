{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module VoidTalon.JSON
  ( ToJSONEncoding (..),
    (.:<>),
    SchemaType (..),
    Schema (..),
    emptySchema,
    ParseEither (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson hiding (toEncoding)
import qualified Data.Aeson as J
import Data.Aeson.Encoding
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Parser)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (maybeToList)
import qualified Data.Text as T

-- We don't specify ToJSON instances because that would require us to also implement `toJSON`,
-- causing code duplication and no advantage.
class ToJSONEncoding a where
  toEncoding :: a -> Encoding

-- This {-# OVERLAPPABLE #-} pragma isn't actually needed, it just prevents a false-positive error
-- with the language server.
instance {-# OVERLAPPABLE #-} (ToJSON a) => ToJSONEncoding a where
  toEncoding = J.toEncoding

-- | Get an optional monoid field from a JSON object, or use mempty if it's missing.
(.:<>) :: (FromJSON a, Monoid a) => Object -> Key -> Parser a
v .:<> k = v .:? k .!= mempty

data SchemaType
  = SchemaTypeObject -- "object"
  | SchemaTypeArray -- "array"
  | SchemaTypeBool -- "boolean"
  | SchemaTypeInt -- "integer"
  | SchemaTypeFloat -- "number"
  | SchemaTypeString -- "string"
  | SchemaTypeNull -- "null"

instance Show SchemaType where
  show SchemaTypeObject = "object"
  show SchemaTypeArray = "array"
  show SchemaTypeBool = "boolean"
  show SchemaTypeInt = "integer"
  show SchemaTypeFloat = "number"
  show SchemaTypeString = "string"
  show SchemaTypeNull = "null"

instance FromJSON SchemaType where
  parseJSON = withText "SchemaType" $ \case
    "object" -> pure SchemaTypeObject
    "array" -> pure SchemaTypeArray
    "boolean" -> pure SchemaTypeBool
    "integer" -> pure SchemaTypeInt
    "number" -> pure SchemaTypeFloat
    "string" -> pure SchemaTypeString
    "null" -> pure SchemaTypeNull
    t -> fail ("Unexpected SchemaType: " ++ T.unpack t)

instance ToJSONEncoding SchemaType where
  toEncoding = J.toEncoding . show

data Schema = Schema
  { types :: [SchemaType],
    -- | Properties, should be set only if `types` contains SchemaTypeObject
    properties :: [(T.Text, Schema)],
    -- | Requried fields, should be set only if `types` contains SchemaTypeObject
    required :: [T.Text],
    description :: Maybe T.Text
  }

emptySchema :: Schema
emptySchema =
  Schema
    { types = [],
      properties = [],
      required = [],
      description = Nothing
    }

instance ToJSONEncoding Schema where
  toEncoding Schema {types, properties, required, description} =
    pairs $
      mconcat $
        [(pair "type" encodedTypes)]
          ++ (if null required then [] else ["required" .= required])
          ++ ( if null properties
                 then []
                 else
                   [ pair "properties" . pairs $
                       foldMap (uncurry pair . bimap fromText toEncoding) properties
                   ]
             )
          ++ (maybeToList $ ("description" .=) <$> description)
    where
      encodedTypes = case types of
        [t] -> toEncoding t
        l -> list toEncoding l

-- | Like Either, but Aeson parses it by trying both variants, preferring the left.
data ParseEither l r = PLeft l | PRight r

instance (FromJSON l, FromJSON r) => FromJSON (ParseEither l r) where
  parseJSON v = (PLeft <$> parseJSON v) <|> (PRight <$> parseJSON v)
