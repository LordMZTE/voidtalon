module VoidTalon.CLI (Arguments (..), PromptOption (..), parser, readArguments) where

import qualified Data.Text as T
import Options.Applicative
import PackageInfo_voidtalon (synopsis)
import qualified VoidTalon.Net.MCP as MCP

data Arguments = Arguments
  { config :: Maybe String,
    mcp :: [String],
    mcpHttp :: [MCP.HTTPConnectionSpec],
    prompt :: PromptOption
  }

data PromptOption = PONone | POText T.Text | POFile FilePath

parser :: Parser Arguments
parser =
  Arguments
    <$> optional
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "CONFIG"
              <> help "Override configuration directory"
          )
      )
    <*> many
      ( strOption
          ( long "mcp"
              <> short 'm'
              <> metavar "COMMAND"
              <> help "Use an MCP server over stdio.  May be passed multiple times.  Accepts a POSIX shell command."
          )
      )
    <*> many
      ( option
          auto
          ( long "mcp-http"
              <> short 'M'
              <> metavar "SPEC"
              <> help "Use an MCP server over HTTP.  May be passed multiple times.  See the wiki for accepted syntax."
          )
      )
    <*> parserPromptOption

parserPromptOption :: Parser PromptOption
parserPromptOption =
  ( POText
      <$> strOption
        ( long "prompt"
            <> short 'p'
            <> metavar "PROMPT_TEXT"
            <> help "Use this prompt, then start."
        )
  )
    <|> ( POFile
            <$> strOption
              ( long "prompt-file"
                  <> short 'P'
                  <> metavar "PROMPT_FILE"
                  <> help "Read a prompt from this file, then start."
              )
        )
    <|> pure PONone

readArguments :: IO Arguments
readArguments = execParser $ info (helper <*> parser) (fullDesc <> progDesc synopsis)
