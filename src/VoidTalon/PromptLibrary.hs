module VoidTalon.PromptLibrary (promptDirname, listPrompts, readPrompt) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.FilePath ((</>))

-- | The name of the directory prompts are stored in inside the config directory
promptDirname :: String
promptDirname = "prompts"

-- | Given the base path to the config directory, lists all available prompts
listPrompts :: FilePath -> IO [String]
listPrompts = listDirectory . (</> promptDirname)

-- | Given a prompt name as returned by @listPrompts@, reads the prompt content.
readPrompt :: FilePath -> String -> IO T.Text
readPrompt dir name = TIO.readFile $ dir </> promptDirname </> name
