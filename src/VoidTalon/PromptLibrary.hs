{-# LANGUAGE TupleSections #-}

module VoidTalon.PromptLibrary
  ( promptDirname,
    listPrompts,
    PromptInfo,
    ExecInfo (..),
    inspectPrompt,
    evalPrompt,
  )
where

import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (Permissions (executable), getPermissions, listDirectory, removeFile)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process (CreateProcess (delegate_ctlc, env), createProcess, proc, waitForProcess)
import VoidTalon.Net.Completions (ReasoningEffort (..))
import VoidTalon.Net.Models (ModelInfo (..), Pricing (..))
import VoidTalon.Util (mkTempFile)

-- | The name of the directory prompts are stored in inside the config directory
promptDirname :: String
promptDirname = "prompts"

-- | Given the base path to the config directory, lists all available prompts
listPrompts :: FilePath -> IO [String]
listPrompts = listDirectory . (</> promptDirname)

type PromptInfo = (FilePath, Bool)

-- | Information that will be passed as environment variables to executable prompt templates.
data ExecInfo = ExecInfo
  { connection :: T.Text,
    model :: Maybe ModelInfo,
    reasoning :: ReasoningEffort
  }

-- | Given a prompt name as returned by @listPrompts@, does an initial read of the prompt, building
-- the file path and checking if it is executable or not.
inspectPrompt :: FilePath -> String -> IO PromptInfo
inspectPrompt dir name = do
  let path = dir </> promptDirname </> name
  perms <- getPermissions path
  pure (path, perms.executable)

-- | Given the tuple returned by inspectPrompt, reads or executes the prompt.
evalPrompt :: PromptInfo -> ExecInfo -> IO T.Text
evalPrompt (path, exec) info =
  if exec
    then do
      tmpfile <- mkTempFile "txt"
      env' <- getEnvironment
      let env =
              ("VOIDTALON_OUTPUT", tmpfile)
              : ("VOIDTALON_CONNECTION", T.unpack info.connection)
              : catMaybes
                [ ("VOIDTALON_MODEL",) . T.unpack . (.id) <$> info.model,
                  ("VOIDTALON_MODEL_DESCRIPTION",) . T.unpack <$> (info.model >>= (.description)),
                  ("VOIDTALON_CONTEXT_LENGTH",) . show <$> (info.model >>= (.contextLength)),
                  ("VOIDTALON_PRICING",) . showPricing <$> (info.model >>= (.pricing)),
                  case info.reasoning of
                    REUnspecified -> Nothing
                    RELlamaCppTokens n -> Just ("VOIDTALON_REASONING", show n)
                    REStr s -> Just ("VOIDTALON_REASONING", T.unpack s)
                ]
              ++ env'
      let spec = (proc path []) {delegate_ctlc = True, env = Just env}
      (Nothing, Nothing, Nothing, pid) <- createProcess spec
      _ <- waitForProcess pid
      output <- TIO.readFile tmpfile
      removeFile tmpfile
      pure output
    else TIO.readFile path
  where
    showPricing Pricing {prompt, completion} = mconcat [T.unpack prompt, ";", T.unpack completion]
