# VoidTalon

> _not a claw_

![demo video](./demo.mp4)

VoidTalon is a lightweight yet feature-complete TUI for chatting with an LLM provided via an
OpenAI-compatible API such as llama.cpp that doesn't claim to be an "aGeNt".

This is not half a million lines of slopware like \*Claw, it's a no-nonsense program written **by a
human** that still aims for feature parity (minus bloat features like a bunch of built-in tools no
one needs and integration into chat software) with such slopware, while actually being usable.

## Comparison with similar projects

Yes, I was having a laugh when I wrote this.

|                       | NanoBot           | ZeroClaw             | OpenClaw                  | VoidTalon  |
| --------------------- | ----------------- | -------------------- | ------------------------- | ---------- |
| **Language**          | ❌ P\*thon        | ❌ Rust              | ❌ TypeShit               | ✅ Haskell |
| **RAM**               | ❌ > 100MB        | ✅ < 5 MB            | ❌ > 1 GB                 | ❌ ~100MB  |
| **Built-in Tools**    | ❌ 19 (??)        | ❌ 25 (???)          | ❌ ∞ (????)               | ✅ 3       |
| **Lines of Code**     | ❌ 191,835 (????) | ❌ 656,863 (???????) | ❌ 5,859,629 (??????????) | ✅ 3300    |
| **Unusable Slopware** | ❔ dunno          | ❌ yes               | ❌ yes                    | ✅ no      |

You might also notice the missing "startup time" row, which is because I didn't bother to measure
it. While this is a large number for some of our competitors (no idea why), it's a completely
ridiculous metric for any sane project. To you VoidTalon, you `cd` into the directory where you
want to work and then run `vt`. There is no noticable delay in startup.

## Features

- [x] Vim-inspired keybindings (not modal yet)
- [x] Tool calling
    - [x] MCP over stdio (HTTP is coming soon)
    - [x] A small, no-nonsense collection of built-in tools (see below)
    - [x] Toggling of individual tools, schema and description viewer
    - [x] User confirmation dialog for all tool calls
    - [x] Manually spoofing tool responses
    - [x] Managing tools, each tool can be individiually enabled and disabled, schema and
          description are shown
- [x] Interactive switching between multiple configured connections (change between OpenRouter,
      llama.cpp and other on the fly!)
- [x] Interactive model selection for all available models offered by the connection
- [x] Interactive editing of messages
- [x] Completions via an OpenAI-compatible API, tested with llama.cpp and OpenRouter
- [x] Reporting of token count and TPS (the latter is only supported on llama.cpp)
- [x] Fancy TUI markdown rendering and syntax highlighting for code blocks
- [x] Add custom system prompts
- [x] Prompt library support for easy insertion of pre-made prompts

## Built-in tools

- `read_file` / `write_file` reading and writing files by path
- `run_command` run a shell command, forwarding stdout and stderr to both the user and the LLM,
  stdin is connected to user for control.

This is open for suggestions, just nothing too bloated (there will be no web search function here,
use an appropriate MCP server!)
