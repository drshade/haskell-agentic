# Changelog for agentic

## 0.2.0.5 - 2026-10-06

* `takeFirst` and `takeSecond` do what `arr fst` and `arr snd` do, but a
  diagram can follow them. `StepInfo` has `FirstHalf` and `SecondHalf` for
  them.
* `:/\` is a pair, as a type and a pattern, so the nested pairs `&&&` builds
  read flat: `\(creature :/\ picture :/\ card) -> ...`.
* `mermaid` and `dot` follow each half of a pair from `&&&` or `***`, so a
  later `first`, `second` or `***` is wired only to the half it gets.
  Previously every step before the pair was wired into both halves.
* A step reached by the same node along several routes, such as both sides of
  a `|||`, gets one edge instead of one per route.
* A `repeatUntil`'s input is drawn going straight out as well as through the
  body, since the condition is checked before the first run.
* `(a &&& b) &&& c` is described as a pair inside a pair, not flattened like
  `a &&& b &&& c`.

## 0.2.0.4 - 2026-10-06

* Record fields are no longer functions: the packages are written with
  `DuplicateRecordFields`, `NoFieldSelectors` and `OverloadedRecordDot`, so
  read a field with record dot (`c.schema`, `map (.label) opts`). Flows that
  read the library's records need `OverloadedRecordDot`, and ones that build
  records with shared field names need `DuplicateRecordFields`.
* Fields lose the prefixes that only kept them unique. `Codec`'s `codecSchema`
  is `schema`; `ObjectCodec`, `Case`, `Option`, `OptionSet`, `Field`,
  `Variant`, `Note`, `ToolInfo`, `FlowGraph`, `Edge`, `Choice`, `Score`,
  `JudgeRequest`, `ToolSpec`, `ToolCall`, `Event`, and `StepInfo`'s
  `DraftInfo` and `JudgeInfo` drop theirs the same way (`caseTag` is `tag`,
  `edgeFrom` is `from`, `requestInput` is `input`). `Instruction`'s field is
  `text`, and `SystemOne`'s and `SystemTwo`'s are both `ask`. `ToolCall`
  keeps `callId`: a field named `id` clashes with the Prelude's under MicroHs.
* `reschema` changes a codec's schema. `c {schema = ...}` is ambiguous now
  that `Field` has a `schema` too.
* `inParallel` and `failWith` use a runtime's `parallel` and `failure`, which
  record dot can't select because they're polymorphic.

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
