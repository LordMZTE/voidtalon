module VoidTalon.TUI.Types
  ( Event (..),
    Name (..),
  )
where

import VoidTalon.Net.Completions (CompletionChoice)

data Event = EvGetCompletions CompletionChoice
data Name = NPromptField | NOutputVP deriving (Eq, Ord, Show)

