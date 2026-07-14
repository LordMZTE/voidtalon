module VoidTalon.Timeline (Entry (..), LLMMessage (..)) where

import qualified Data.Text as T

data Entry
  = PromptEntry T.Text
  | OutputEntry LLMMessage

data LLMMessage = LLMMessage
  { reasoning :: T.Text,
    content :: T.Text
  }

instance Semigroup LLMMessage where
  (LLMMessage aR aC) <> (LLMMessage bR bC) = LLMMessage (aR <> bR) (aC <> bC)

instance Monoid LLMMessage where
  mempty = LLMMessage T.empty T.empty
