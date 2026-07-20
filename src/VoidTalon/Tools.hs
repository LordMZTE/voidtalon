{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Tools
  ( Schema (..),
    NamedDescription (..),
    Description (..),
    Plan,
    Invocation,
    Tool (..),
    Call (..),
  )
where

import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding (list, pair)
import Data.Aeson.Key (fromText)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import VoidTalon.Util (ToJSONEncoding (..))
import Control.Applicative ((<|>))

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
          pair "required" (list toEncoding required),
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

-- | A description that tells the LLM what this tool does and how to use it.
data Description = Description
  { description :: T.Text,
    schema :: Schema
  }

data NamedDescription = NamedDescription T.Text Description

instance ToJSONEncoding NamedDescription where
  toEncoding (NamedDescription name Description {description, schema}) =
    pairs $
      "type" .= ("function" :: T.Text)
        <> pair
          "function"
          ( pairs $
              mconcat
                [ "name" .= name,
                  "description" .= description,
                  pair "parameters" (toEncoding schema)
                ]
          )

-- | A list of human-readable key-value pairs explaining what the tool call will do.
type Plan = [(T.Text, T.Text)]

-- | A pair consisting of a plan for invoking a tool and an action that runs the tool and returns
-- some output to give to the agent.
type Invocation = (Plan, IO LT.Text)

data Tool = Tool
  { description :: Description,
    -- | Called when the LLM wants to run this tool.  This is given the parameters to call with,
    -- which hopefully matches the schema from @description@, and returns an invocation or an error
    -- if the parameters were invalid.
    invoke :: Value -> Result Invocation
  }

data Call = Call
  { id :: Maybe T.Text,
    name :: T.Text,
    parameters :: T.Text
  }

instance Semigroup Call where
  Call id1 name1 params1 <> Call id2 name2 params2 = Call (id2 <|> id1) (name1 <> name2) (params1 <> params2)

instance Monoid Call where
  mempty = Call Nothing T.empty T.empty
