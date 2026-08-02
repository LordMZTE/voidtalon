module VoidTalon.CLI (Arguments (..), parser, readArguments) where

import Options.Applicative
import PackageInfo_voidtalon (synopsis)

data Arguments = Arguments
  { config :: Maybe String,
    mcp :: [String]
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
    <*> many
      ( strOption
          ( long "mcp"
              <> short 'm'
              <> metavar "COMMAND"
              <> help "Use an MCP server over stdio.  May be passed multiple times.  Accepts a POSIX shell command."
          )
      )

readArguments :: IO Arguments
readArguments = execParser $ info (helper <*> parser) (fullDesc <> progDesc synopsis)
