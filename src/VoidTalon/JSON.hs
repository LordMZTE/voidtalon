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
  )
where

import Data.Aeson hiding (toEncoding)
import qualified Data.Aeson as J
import Data.Aeson.Encoding
import Data.Aeson.Key (fromText, toText)
import Data.Aeson.KeyMap (toList)
import Data.Aeson.Types (Parser, emptyObject)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Control.Applicative ((<|>))

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
  | SchemaTypeBool -- "boolean"
  | SchemaTypeInt -- "integer"
  | SchemaTypeString -- "string"
  | SchemaTypeNull -- "null"

instance Show SchemaType where
  show SchemaTypeObject = "object"
  show SchemaTypeBool = "boolean"
  show SchemaTypeInt = "integer"
  show SchemaTypeString = "string"
  show SchemaTypeNull = "null"

instance FromJSON SchemaType where
  parseJSON = withText "SchemaType" $ \case
    "object" -> pure SchemaTypeObject
    "boolean" -> pure SchemaTypeBool
    "integer" -> pure SchemaTypeInt
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

instance FromJSON Schema where
  parseJSON (Object v) = Schema
    <$> ((pure <$> v .: "type") <|> v .: "type")
    <*> (v .:? "properties" .!= emptyObject >>= parseProperties)
    <*> (v .:<> "required")
    <*> (v .:? "description")
    where
      parseProperties :: Value -> Parser [(T.Text, Schema)]
      parseProperties (Object p) =
        sequenceA $ (\(k, v') -> (toText k,) <$> parseJSON v') <$> toList p
      parseProperties _ = mempty
    
  parseJSON _ = mempty

