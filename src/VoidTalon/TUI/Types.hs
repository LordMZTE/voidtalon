module VoidTalon.TUI.Types
  ( Event (..),
    Name (..),
  )
where

import VoidTalon.Timeline (LLMMessage)

data Event = EvGetCompletions LLMMessage
data Name = NPromptField | NOutputVP deriving (Eq, Ord, Show)

