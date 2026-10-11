{-# LANGUAGE TupleSections #-}

module VoidTalon.PromptLibrary
  ( promptDirname,
    listPrompts,
    PromptInfo,
    ExecInfo (..),
    inspectPrompt,
    inspectPromptFile,
    evalPrompt,
  )
where

import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

-- | Like @inspectPromptFile@, but instead takes the config directory and the name of a prompt as
-- returned by @listPrompts@.
inspectPrompt :: FilePath -> String -> IO PromptInfo
inspectPrompt dir name = inspectPromptFile $ dir </> promptDirname </> name

-- | Given a path to a prompt template, does an initial read of the prompt, checking if it is
-- executable or not.
inspectPromptFile :: FilePath -> IO PromptInfo
inspectPromptFile path = do
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
      output <- BS.readFile tmpfile
      removeFile tmpfile
      pure $ T.decodeUtf8 output
    else T.decodeUtf8 <$> BS.readFile path
  where
    showPricing Pricing {prompt, completion} = mconcat [T.unpack prompt, ";", T.unpack completion]
