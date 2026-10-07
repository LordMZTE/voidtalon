{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VoidTalon.Tools
  ( NamedDescription (..),
    Description (..),
    Plan,
    Invocation,
    Tool (..),
    CallID,
    Call (..),
    postProcessToolOutput,
    State (..),
    stateEnabledL,
    stateAutoconfirmL,
    mkState,
    Group (..),
    groupOpenedL,
    groupStatesL,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson hiding (toEncoding)
import Data.Aeson.Encoding (pair)
import Data.Char (isSpace)
import qualified Data.Text as T
import Lens.Micro.TH (makeLensesFor)
import VoidTalon.JSON (Schema, ToJSONEncoding (..))

-- | A description that tells the LLM what this tool does and how to use it.
data Description = Description
  { -- | An explanation of what the tool does.  Must not contain ANSI escapes or carriage returns.
    description :: T.Text,
    -- | For why this isn't a `Schema`, refer to the rand in the MCP code.
    schema :: Either Schema Value
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
                  pair
                    "parameters"
                    ( case schema of
                        Left s -> toEncoding s
                        Right e -> toEncoding e
                    )
                ]
          )

-- | A list of human-readable key-value pairs explaining what the tool call will do.
type Plan = [(T.Text, T.Text)]

-- | A pair consisting of a plan for invoking a tool and an action that runs the tool and returns
-- some output to give to the agent.
type Invocation = (Plan, IO T.Text)

data Tool = Tool
  { description :: Description,
    -- | Called when the LLM wants to run this tool.  This is given the parameters to call with,
    -- which hopefully matches the schema from @description@, and returns an invocation or an error
    -- if the parameters were invalid.
    invoke :: T.Text -> Either String Invocation
  }

type CallID = Maybe T.Text

data Call = Call
  { id :: CallID,
    name :: T.Text,
    parameters :: T.Text
  }

instance Semigroup Call where
  Call id1 name1 params1 <> Call id2 name2 params2 = Call (id2 <|> id1) (name1 <> name2) (params1 <> params2)

instance Monoid Call where
  mempty = Call Nothing T.empty T.empty

-- | This should be called on tool output before passing the result to the LLM.  This mostly handles
-- empty/whitespace-only output, since that usually causes confusion.
postProcessToolOutput :: T.Text -> T.Text
postProcessToolOutput = \case
  res | T.null res -> "<no tool output>"
  res | T.all isSpace res -> "<only whitespace in tool output>"
  res -> res

-- | Current state of a registered tool.
-- (enabled, name, tool)
data State = State
  { enabled :: Bool,
    autoconfirm :: Bool,
    name :: T.Text,
    tool :: Tool
  }

makeLensesFor
  [ ("enabled", "stateEnabledL"),
    ("autoconfirm", "stateAutoconfirmL")
  ]
  ''State

-- | Constructs an initial state from a name and tool, with it being disabled.
mkState :: T.Text -> Tool -> State
mkState name tool = State {enabled = False, autoconfirm = False, name, tool}

-- | A group of tools.  We use one for built-in tools and one per MCP server.
-- type ToolGroup = (Bool, T.Text, [ToolState])
data Group = Group
  { -- | If this group is folded or opened in the tool manager UI
    opened :: Bool,
    name :: T.Text,
    description :: Maybe T.Text,
    states :: ![State]
  }

makeLensesFor
  [ ("opened", "groupOpenedL"),
    ("states", "groupStatesL")
  ]
  ''Group
