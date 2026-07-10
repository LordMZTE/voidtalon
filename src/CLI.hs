module CLI (Arguments (..), parser, readArguments) where

import Options.Applicative

data Arguments = Arguments
  { config :: Maybe String
  }

parser :: Parser Arguments
parser =
  Arguments
    <$> optional
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "CONFIG"
              <> help "Override configuration file"
          )
      )

readArguments :: IO Arguments
readArguments =
  execParser $ info (helper <*> parser) (fullDesc <> progDesc desc)
  where
    desc = "Lean TUI for OpenAI-compatible LLMs with MCP and tools."
