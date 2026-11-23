# Await representation spike (#14)

This branch is an experiment from main `0d764e6`, not a feature PR. It adds
minimal Await/Wait/Suspended constructors and two copies of the interpreter
worker. Both run real Agentic flows, use the existing static planner and
journal hooks, and retain the existing tool and recursion machinery.

- `SpikeExplicit`: `m (Either Waits a)` with explicit result operations.
- `SpikeTransformer`: `ExceptT Waits m` inside the interpreter only. User
  actions, runtime hooks and the journal still run in the original `m`.

Both use the existing Scope protocol. The protected body returns its normal
success containing either waits or a value. The normal terminal callback then
emits Suspended or Returned. A suspension report failure propagates like a
normal-return report failure; it is not swallowed by failure-report handling.
No new Scope API or suspension hooks are needed for these prototypes.

The collector runs every independent branch with the existing parallel hook,
then combines successful results or pending frontiers. Real monad failures
still escape through the existing parallel and Scope implementations. Wait
lists concatenate right-associatively; the first prototype's left fold was
quadratic and was corrected before the recorded benchmark.

## Behaviour checked

`Main.hs` includes a pure request Program with store and observation requests,
wrapped in `ExceptT FetchError`. The Program has no suspension operation and
no native exceptions. Both representations pass:

- three waits, one reply followed by two waits, then complete with typed replies;
- canonical numeric reply and invalid reply validation;
- sequential awaits exposing only the reachable frontier;
- a separate FetchError and observer FetchError remaining failures;
- an independent durable sibling saving its result;
- two tool calls with the same provider ID exposing distinct waits;
- recursive tool invocations waiting and resuming through saved model turns;
- IO observer cancellation escaping, with parent Cancelled;
- a concurrent durable sibling completing after another child suspended.

The reply helper here takes the actual codec directly. This is enough to test
representation behaviour, not the full fulfil API. Structural codec lookup,
full pending metadata/payload, fresh-process fulfil, rendering, named public
errors, nested interpret boundary, and full regression coverage belong to the
feature implementation. Await has a deliberately temporary renderer label and
unsupported error in the original interpreter. Do not merge this branch.

## Measurements and recommendation

`bench.csv` contains elapsed time and allocation, GHC 9.14.1 `-O1`, 5,000
elements, median of seven samples with rotated runner order, `+RTS -T -N1`.
Setup and warming of journal hits are outside the measured region. Each sample
forces its final results, or the number of waits. Allocation is sampled across
forced collections; time excludes those forced collections. The store is an
MVar-protected in-memory Map, not disk/network. Concurrent cases use the real
mapConcurrently runtime on one capability, so they measure concurrent
scheduling and journal contention, not multicore scaling.

The in-process `main` runner is the existing interpreter, with only an Await
unsupported case and node-kind case added to accommodate the spike syntax.
The separate observation comparison uses an untouched `0d764e6` worktree.

| Case | Existing worker ms | Explicit ms | Transformer ms |
| --- | ---: | ---: | ---: |
| Sequential journal, cheap steps | 1.569 | 1.866 | 1.794 |
| Sequential durable misses | 22.368 | 23.109 | 23.204 |
| Sequential durable hits | 11.338 | 11.511 | 11.595 |
| Sequential waiting | n/a | 4.408 | 4.357 |
| Concurrent journal, cheap steps | 16.971 | 16.871 | 17.370 |
| Concurrent durable misses | 72.030 | 70.360 | 73.623 |
| Concurrent durable hits | 28.590 | 29.500 | 29.782 |
| Concurrent waiting | n/a | 17.767 | 17.272 |

Cheap sequential journaled plumbing is about 14% slower with the transformer,
19% with explicit results. Transformer allocations rise from 18.269 to 21.669
MB (~19%) there. The measured durable cases add roughly 2-4% time with the
transformer and 1-5% allocation; these local samples are not a general bound.
Representation differences are small enough that they should not drive the
API. No journal is read for pure plumbing in any of these cases.

Running the waiting worker even without a journal regresses cheap steps.
Dispatching no-journal runs to the existing worker restores equal allocations
(18.269 MB) and timings within noise (1.558 ms existing vs 1.566 explicit-gated
and 1.553 transformer-gated). `Observations.hs` additionally checks that dispatch
against an untouched main build with the existing 10,000-element benchmark;
see `observation-paired.txt`.

Recommendation: internal transformer, with a specialised no-journal path.
Use the standard transformer instances instead of maintaining a custom result
algebra. The spike duplicates workers to isolate the comparison; production
should share the interpreter logic if specialization can preserve these costs.
That factoring needs its own benchmark check during implementation. Do not
silently accept a no-journal regression or claim a zero-cost journaled path.

## Reproduce

From this spike branch:

```sh
cabal build agentic-io --offline
cabal exec --offline -- ghc -O1 -threaded -rtsopts -Wall -Wno-name-shadowing \
  -package agentic -package agentic-io -package async -package containers \
  -package transformers spikes/await-14/Main.hs -o /tmp/await-spike
/tmp/await-spike
/tmp/await-spike bench +RTS -T -N1
```

For the observation guard, build the existing observation-overhead benchmark
on main in a separate worktree and alternate that executable with:

```sh
cabal exec --offline -- ghc -O1 -XGHC2021 -XOverloadedStrings \
  -XOverloadedRecordDot -package agentic spikes/await-14/Observations.hs \
  -o /tmp/await-observation
/tmp/await-observation
```
