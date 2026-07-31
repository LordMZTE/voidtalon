{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module VoidTalon.JSON
  ( ToJSONEncoding (..),
    (.:<>),
    Schema (..),
  )
where

import Data.Aeson hiding (toEncoding)
import qualified Data.Aeson as J
import Data.Aeson.Encoding
import Data.Aeson.Key (fromText, toText)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Data.Aeson.KeyMap (toList)

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

data Schema
  = SchemaObject
      { properties :: [(T.Text, Schema)],
        required :: [T.Text]
      }
  | SchemaBool {description :: T.Text}
  | SchemaInteger {description :: T.Text}
  | SchemaString {description :: T.Text}

encodeSimpleSchema :: T.Text -> T.Text -> Encoding
encodeSimpleSchema typ desc = pairs ("type" .= typ <> "description" .= desc)

instance ToJSONEncoding Schema where
  toEncoding SchemaObject {properties, required} =
    pairs $
      mconcat
        [ "type" .= ("object" :: T.Text),
          pair "required" (list J.toEncoding required),
          pair
            "properties"
            ( pairs $
                mconcat $
                  (\(k, v) -> pair (fromText k) (toEncoding v)) <$> properties
            )
        ]
  toEncoding SchemaBool {description} = encodeSimpleSchema "boolean" description
  toEncoding SchemaInteger {description} = encodeSimpleSchema "integer" description
  toEncoding SchemaString {description} = encodeSimpleSchema "string" description

instance FromJSON Schema where
  parseJSON (Object v) =
    (v .: "type" :: Parser T.Text) >>= \case
      "object" -> SchemaObject <$> (v .: "properties" >>= parseProperties) <*> (v .: "required")
      "boolean" -> SchemaBool <$> (v .: "description")
      "integer" -> SchemaInteger <$> (v .: "description")
      "string" -> SchemaString <$> (v .: "description")
      _ -> mempty
    where
      parseProperties :: Value -> Parser [(T.Text, Schema)]
      parseProperties (Object p) = 
        sequenceA $ (\(k, v') -> (toText k,) <$> parseJSON v') <$> toList p
      parseProperties _ = mempty

  parseJSON _ = mempty
