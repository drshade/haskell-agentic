# Changelog

All notable changes to this project will be documented in this file.

## 0.1.0.0 — 2026-03-12

### Added
- Kleisli arrow-based agent framework (`Agentic`, `AgenticRWS`)
- Type-safe LLM extraction via Dhall schema (`extract @MyType`)
- JSON Schema support via autodocodec (`extractWith @Json`)
- `SchemaFormat` typeclass for pluggable schema backends
- Concurrent fanout combinator (`<<.>>`)
- Schema-injecting sequential combinator (`>...>`)
- `AgenticError` type with `ParseError`, `LLMError`, `SchemaError`
- `RetryConfig` and `withRetry` for configurable retry on parse failures
- `LLMProvider` and `LLMConfig` for configurable LLM backend
- `LLM.Provider`: `defaultConfig`, `providerModel`
- Terminal progress display with LLM call count reporting
- Anthropic Claude API integration
