{-# LANGUAGE OverloadedStrings #-}

module VoidTalon.Config.Example (exampleConfig) where

import qualified Data.Text as T

-- | The example configuration file that we drop off when there isn't one.
exampleConfig :: T.Text
exampleConfig =
  T.unlines
    [ "# You can specify as many connections as you want, but you must have at least one.  The first",
      "# connection will be the default that's active at startup.",
      "[[connections]]",
      "name = \"llama.cpp\" # Mandatory, Display name of the connection",
      "# Mandatory, The URL of an OpenAI-compatible API, including the version number.",
      "base_url = \"http://127.0.0.1:8080/v1\"",
      "# Optional, a map of HTTP headers to pass to the server, for authorization or other purposes",
      "#headers = { Authorization = \"...\" }",
      "# Optional, the model to use by default.  If this is left unspecified, you will need to select",
      "# a model after startup using the model selector",
      "#default_model = \"my-model\"",
      "",
      "# Example for what a config for OpenRouter could look like",
      "#[[connections]]",
      "#name = \"OpenRouter\"",
      "#base_url = \"https://openrouter.ai/api/v1\"",
      "#headers = { Authorization = \"Bearer sk-or-v1-XXX\" }",
      "#default_model = \"openrouter/free\""
    ]
