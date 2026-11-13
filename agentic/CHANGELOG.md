# Changelog for agentic

## Unreleased

* Replace the old event envelope with occurrence keys, explicit parents, loop
  and call frames, lifecycle outcomes, selections, and resolved tool targets.
  `EventPayload` is optional; `summarise` and `summaryValue` expose portable
  metadata without retaining payload values.
* Add runtime `Scope` hooks. IO distinguishes failed and cancelled exits while
  preserving exception types; other monads can supply their own hook. Returns
  remain lazy, and interrupted observers leave unknown outcomes without retries.
* Preserve addresses in `Located` descriptions before display projection.
  Graphs publish address/item and body-root relations, finite tool bodies,
  recursive reference edges, and logical choice ids on rendered edges.
* Add test-only replay checks and an observation-overhead benchmark. No event
  recorder, transport, clock or production reducer is added.

* Add `recursiveTool` and `recursiveTools` for self and mutual recursion, with
  abstract tools and tuple binders. Execute real members against a finite static
  cursor; tools saved through effects retain their original definitions.
* Preflight validates all real group members before effects. Both preflight and
  description reject static tool-body nesting beyond 100; this is a heuristic
  limit, not cycle detection, and never limits explicit runtime recursion.
* `describe` now returns `Either FlowError Description`. It remains independent
  of model-codec compilation. Tool descriptions/JSON contain finite group
  definitions and explicit references; same-name bodies are not deduplicated.
  Use smart constructors instead of the now-internal `Tool` constructors;
  handwritten tool contracts go through `Contract` instances.
* Recursive builders must return concrete definitions and have productive
  metadata. Member aliases report a clear error; arbitrary cyclic metadata
  computations remain partial.

## 0.2.1.0 - 2026-11-06

* `Contract` now produces a typed `Structure`, shared by generic derivation
  and handwritten builders. `outline` exposes nominal/applied identities,
  constructor/field order and style, and extensible scalar parameters.
* `compileModel` and open typed `compileWith` produce fallible codecs. Scalar
  definitions can supply model defaults; independent adapters can override both
  scalar and structural representations, including upstream types.
* Flow interpretation preflights all contracts and tool bodies before effects
  or provider calls. Unsupported projections raise structured
  `UnsupportedContract [Note] ProjectionError` failures;
  discovery remains usable. Lossy nested model optionals are rejected.
* Structural mappings use `mapStructure`; compiled mappings retain `mapCodec`.
  Replace `deriving via Enumeration T` with `contract = enumeration` in an
  explicit `Contract T` instance. Flow descriptions now carry `Outline`.
  Direct codec consumers must call `compileModel` (or another interpreter)
  before accessing `.schema`, `.encode`, or `.decode`. `reschema` modifies
  compiled codecs; `documented`, `field`, and `checked` modify contract trees.
  Generic traversal helpers now produce typed trees, cases and fields rather
  than compiled codecs. Handwritten nominal contracts can use
  `nominal`; generic derivation preserves their identity automatically.
* Expose compiled builders in `Agentic.Codec` and reusable
  `compileFieldsWith`/`compileCaseWith` helpers. Sum cases use existential payloads
  with total matching; singleton constructors retain product structure.
* Retire MicroHs support in favor of GHC generics and type evidence. Keep the
  handwritten-contract workflow suite on GHC.
* Add independent-projection acceptance fixtures under `agentic/test`, covering
  exact values, malformed scalars, nested containers and preflight failures.
* `Outline` omits representation-only mappings. `OutlineField`, `OutlineCase`
  and existential `Case` expose named record fields. Applicative fields use
  `Pure`, `Apply` and `Field`; compiled sum cases are `CaseCodec`.
* `field` applies its annotation immediately and reports an absent field at
  first evaluation. Codec-level annotation/check builders are no longer public.
* Share the test-fixture `modelContract` helper through `Agentic.Scripted`.

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
