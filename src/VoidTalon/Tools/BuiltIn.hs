{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Tools.BuiltIn (builtinTools) where

import qualified Data.Text as T
import qualified VoidTalon.Tools as Tools
import qualified VoidTalon.Tools.BuiltIn.ReadFile
import qualified VoidTalon.Tools.BuiltIn.RunCommand
import qualified VoidTalon.Tools.BuiltIn.WriteFile

builtinTools :: [(T.Text, Tools.Tool)]
builtinTools =
  [ ("read_file", VoidTalon.Tools.BuiltIn.ReadFile.tool),
    ("write_file", VoidTalon.Tools.BuiltIn.WriteFile.tool),
    ("run_command", VoidTalon.Tools.BuiltIn.RunCommand.tool)
  ]
