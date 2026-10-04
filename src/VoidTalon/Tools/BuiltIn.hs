{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Tools.BuiltIn (builtinGroup, builtinTools) where

import qualified Data.Text as T
import qualified VoidTalon.Tools as Tools
import qualified VoidTalon.Tools.BuiltIn.ReadFile
import qualified VoidTalon.Tools.BuiltIn.RunCommand
import qualified VoidTalon.Tools.BuiltIn.WriteFile

builtinGroup :: Tools.Group
builtinGroup = Tools.Group {
    opened = False,
    name = "Built-in",
    description = Just "VoidTalon's built-in tools.",
    states = uncurry Tools.mkState <$> builtinTools
  }

builtinTools :: [(T.Text, Tools.Tool)]
builtinTools =
  [ ("read_file", VoidTalon.Tools.BuiltIn.ReadFile.tool),
    ("write_file", VoidTalon.Tools.BuiltIn.WriteFile.tool),
    ("run_command", VoidTalon.Tools.BuiltIn.RunCommand.tool)
  ]
