# Revision history for haskell-agentic

## [0.1.0.0] - 2026-03-13

### Added
- `effectful-core` discrete effects replacing monolithic `MonadRWS` constraint
- `Agent es a b = a -> Eff es b` plain function type replacing Kleisli arrows
- `LLM` effect with provider interpreters (`runAnthropic`) — swap providers by changing one line
- `extract`/`extractDhall` with built-in 3-attempt retry and typed error feedback
- `extractJson` using JSON Schema via `autodocodec`
- `SchemaFormat` GADT for explicit format selection
- Typed error ADTs: `SchemaError`, `LLMError`, `ToolError` — no `Text` catch-alls
- Fully typed `Tool` GADT with `mkTool` smart constructor (schema derived from types)
- `AgentEvents` observability effect with structured `Event` ADT
- Mock LLM interpreter for testing without network calls
- `fanoutMap` for concurrent agent fanout
- Dino project example demonstrating full pipeline composition

### Changed
- Package restructured from executable to library for Hackage release
- Module hierarchy reorganised under `Agentic.*` namespace
