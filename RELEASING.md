# Releasing

All six packages share one version and release together. Pushing a version tag
publishes them to Hackage, so the checks happen before the tag: here, and again
in the `Hackage release` workflow's guards.

## Procedure

1. **Preview as candidates.** Run the `Hackage release` workflow by hand
   (Actions -> Hackage release -> Run workflow). It does everything a release
   does, but uploads the six packages and their docs as candidates, at

   ```
   https://hackage.haskell.org/package/<package>-<version>/candidate
   ```

   Check the READMEs and module docs rendered. Candidates can be deleted, or
   promoted to a release, from those pages.

2. **Stamp the date.** Change each package's pending CHANGELOG entry from
   `## X.Y.Z.W - ???` to the release date (`## 0.2.0.0 - 2026-10-01`), and check
   every cabal file has the version you mean to release. Commit and push.

3. **Tag.** On `main`:

   ```bash
   git tag v0.2.0.0
   git push origin v0.2.0.0
   ```

   The tag must be `v` followed by the exact cabal version.

4. **Wait for the workflow.** It refuses to go on if the tag and any cabal
   version disagree, or if any CHANGELOG entry is still `???`. Otherwise it runs
   `cabal check` on each package, builds and tests all six source tarballs
   together in a clean directory, builds the docs with
   `--haddock-for-hackage`, and publishes everything in dependency order.

Hackage versions cannot be deleted, only deprecated. If a release turns out to
be broken, deprecate it on Hackage and ship a fixed version.

## Setup (once)

The workflow needs a `HACKAGE_AUTH_TOKEN` repository secret: a Hackage API token
from <https://hackage.haskell.org/users/account-management>, added under the
repository's *Settings -> Secrets and variables -> Actions*.

## Doing it by hand

The workflow is a thin wrapper over standard cabal commands:

```bash
for p in agentic agentic-aeson agentic-io agentic-jev agentic-anthropic agentic-openai; do (cd $p && cabal check); done
cabal sdist agentic agentic-aeson agentic-io agentic-jev agentic-anthropic agentic-openai --output-directory dist-release
cabal haddock agentic agentic-aeson agentic-io agentic-jev agentic-anthropic agentic-openai --haddock-for-hackage --enable-doc
# then, for each package in that order:
cabal upload --publish --token "$HACKAGE_AUTH_TOKEN" dist-release/<package>-<version>.tar.gz
cabal upload --publish --token "$HACKAGE_AUTH_TOKEN" --documentation dist-newstyle/<package>-<version>-docs.tar.gz
```

Drop `--publish` to upload candidates instead.
