module VoidTalon.Timeline (Entry (..), LLMMessage (..)) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
import qualified VoidTalon.Tools as Tools

data Entry
  = PromptEntry T.Text
  | OutputEntry LLMMessage

data LLMMessage = LLMMessage
  { reasoning :: T.Text,
    content :: T.Text,
    toolCalls :: IntMap.IntMap Tools.Call
  }

instance Semigroup LLMMessage where
  (LLMMessage aR aC aT) <> (LLMMessage bR bC bT) =
    LLMMessage
      (aR <> bR)
      (aC <> bC)
      (IntMap.unionWith (<>) aT bT)

instance Monoid LLMMessage where
  mempty = LLMMessage T.empty T.empty IntMap.empty
