# Changelog for agentic

## 0.2.0.3 - 2026-10-06

* `fromBasisPoints` is renamed `toProbability`, the inverse of `probability`:
  it takes a probability such as 0.9. `fromBasisPoints` now takes basis points,
  the inverse of `basisPoints`.
* A step's input is called its input everywhere, not its state: the
  `Conversation` fields are `input`, `inputSchema` and `outputSchema`,
  `JudgeRequest`'s is `requestInput`, and `JudgeInfo`'s is `judgeInput`.
* Attaching a description is `documented…` throughout: `documentSchema` is
  `documentedSchema`, and `described` is `documentedOptions`.
* `Agentic.Scripted.alwaysYes` is `fixedAnswers`; it gives yes/no questions
  whatever probability it's given.
* `Agentic.Describe.toValue` is `descriptionValue`.
* `endpoint` takes `Text`, like the other settings.

## 0.2.0.2 - 2026-10-01

* The core now builds and runs under MicroHs as well as GHC. Generic
  deriving of `Contract` and `Options` is GHC only; under MicroHs, write
  contracts with `record`, `required`, `sumOf` and `constructor`.
* `option` takes an option's label from `show` instead of Generics, so it works
  on both compilers. For an enumeration the label is still the constructor's
  name.
* `Tool`'s constructor is positional; `toolName` and `toolDescription` are
  functions.
* `Agentic` no longer exports the constructors of `Shape` and `Variant`, which
  clashed with users' own types. Provider code imports them from
  `Agentic.Schema`.
* A new `agentic-portable-test` suite, which CI also runs under MicroHs.

## 0.2.0.1 - 2026-10-01

* The README now appears on the Hackage package page.

## 0.2.0.0 - 2026-10-01

First release of the v2 design.
