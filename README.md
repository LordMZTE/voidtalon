# VoidTalon

> _not a claw_

---

VoidTalon is a lightweight yet feature-complete TUI for chatting with an LLM provided via an
OpenAI-compatible API such as llama.cpp that doesn't claim to be an "aGeNt".

This is not half a million lines of slopware like \*Claw, it's a no-nonsense program written **by a
human** that still aims for feature parity (minus bloat features like a bunch of built-in tools no
one needs and integration into chat software) with such slopware, while actually being usable.

## Comparison with similar projects

Yes, I was having a laugh when I wrote this.

|                       | NanoBot           | ZeroClaw             | OpenClaw                  | VoidTalon   |
| --------------------- | ----------------- | -------------------- | ------------------------- | ----------- |
| **Language**          | ❌ P\*thon        | ❌ Rust              | ❌ TypeShit               | ✅ Haskell  |
| **RAM**               | ❌ > 100MB        | ✅ < 5 MB            | ❌ > 1 GB                 | ❌ ~13MB    |
| **Built-in Tools**    | ❌ 19 (??)        | ❌ 25 (???)          | ❌ ∞ (????)               | ✅ 1 (TODO) |
| **Lines of Code**     | ❌ 191,835 (????) | ❌ 656,863 (???????) | ❌ 5,859,629 (??????????) | ✅ 1500     |
| **Unusable Slopware** | ❔ dunno          | ❌ yes               | ❌ yes                    | ✅ no       |

You might also notice the missing "startup time" row, which is because I didn't bother to measure
it. While this is a large number for some of our competitors (no idea why), it's a completely
ridiculous metric for any sane project. To you VoidTalon, you `cd` into the directory where you
want to work and then run `vt`. There is no noticable delay in startup.
