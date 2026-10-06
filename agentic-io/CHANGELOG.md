# Changelog for agentic-io

## 0.2.0.5 - 2026-10-06

* No changes; released alongside agentic 0.2.0.5.

## 0.2.0.4 - 2026-10-06

* Follows agentic's record field changes; no API change.

## 0.2.0.3 - 2026-10-06

* `StoreMiss` is a constructor of a new `StoreError`, whose other constructor,
  `StoreUnreadable`, is raised for a recording that can't be read back
  (previously the core's `MalformedAnswers`).
* Recordings key a request by its `input`, `inputSchema` and `outputSchema`, so
  recordings from earlier versions miss.

## 0.2.0.2 - 2026-10-01

* No changes; released alongside agentic 0.2.0.2.

## 0.2.0.1 - 2026-10-01

* No changes; released alongside agentic 0.2.0.1.

## 0.2.0.0 - 2026-10-01

First release of the v2 design.
