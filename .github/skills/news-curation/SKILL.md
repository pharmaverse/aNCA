---
name: news-curation
description: >
  Decide whether a change belongs in NEWS.md, and how to phrase it. Use whenever
  opening or updating a pull request, or any time you are about to add, edit, or
  review a NEWS.md entry.
---

# NEWS.md curation

`NEWS.md` is the **user-facing changelog**. It answers one question for someone
upgrading the package: *"What changed that I can observe or should care about?"*
It is not a commit log and not an issue tracker. Keep it short, terse, and
grouped.

Run this check every time you open or update a PR, before finalizing the NEWS
section.

## Decision gate

Apply the gate in order. **Q1 is decisive**; Q2–Q4 refine the borderline cases.

### Q1 — Is the change observable by a user? (hard gate)

A "user" is either:
- an **end user** of the Shiny app (sees a feature, a fixed bug, changed output,
  a new option), **or**
- a **programmatic caller** of an exported (`@export`) function whose documented
  behaviour, signature, return shape, or defaults changed.

- **Clear NO** → do **not** add a NEWS entry. Stop here. The change is captured
  well enough by the commit message and the linked issue/PR.
- **Clear YES** → it is a candidate; continue to Q2–Q4 to decide grouping and
  wording.
- **Unsure** → continue; Q2–Q4 usually resolve it.

Internal-only changes never pass Q1: refactors, renames, test-only changes,
internal helper fixes, return values consumed only by index internally, CI/tooling,
and formatting.

### Q2 — Is it a regression from this development cycle, or a pre-existing defect?

- If the bug **already existed in the last release** (check with
  `git show <last-tag>:path` — see below) and the surrounding feature also
  already shipped, then nothing changed *for an upgrading user* → lean **drop**.
- Exception: if the surrounding **feature is new/unreleased** this cycle, fold the
  fix silently into that feature's entry instead of adding a separate bug line.

### Q3 — Can it fold into an existing entry?

If another entry in the current `(development version)` block covers the same
feature/area, **merge** rather than adding a new bullet. Prefer a few terse,
grouped bullets over a long list the user won't read. One issue/theme should not
produce many near-duplicate lines.

### Q4 — Correctness / safety carve-in (overrides Q2/Q3, not Q1)

Even if rare or pre-existing, **keep** an entry when the bug could have produced
**silently wrong results, data corruption, or misleading output** — upgraders
need to know to re-check outputs. This matters for a clinical NCA tool. This
carve-in never overrides a clear Q1 "no" (a truly invisible change stays out).

## Quick reference

| Situation | Action |
|---|---|
| New feature / new user-visible option | Add entry |
| Fixed bug a user could observe in the app | Add entry |
| Changed behaviour/signature/return of an exported fn | Add entry |
| Internal refactor, rename, test-only, tooling | No entry |
| Return-shape fix consumed only internally by index | No entry |
| Pre-existing bug, feature already shipped, invisible | No entry |
| Pre-existing bug inside a new unreleased feature | Fold into feature entry |
| Several fixes for the same theme/issue | Group into terse bullets |
| Rare but silently-wrong-results / data bug | Add entry (Q4) |

## How to check if a change predates the last release

```bash
git tag                                  # find the latest release tag
git show <last-tag>:R/<file>.R | grep -n "<the buggy line/pattern>"
git log --oneline -S "<code string>" -- R/<file>.R   # when was it introduced
```

If the buggy code is already present at the last tag, treat it as pre-existing
(apply Q2).

## Wording rules

- Write for the user, describe the **observable effect**, not the implementation.
- One line per change. Terse. No "comprehensive/robust/powerful".
- Reference the PR number: `(#1234)`. Use the PR number, not the issue number,
  unless the repo convention says otherwise.
- Put it under the correct heading (`## New Features`, `## Bug Fixes`, etc.) in the
  `# aNCA (development version)` block.
- Do not restate the same change in multiple bullets.

### Examples

Good (observable, terse):

```markdown
* Summary-exclusion flags (`PKSUMXF`/`PPSUMXF`) no longer hide records from
  individual concentration plots (`pkcg01`) (#1439)
```

Drop (internal return shape, consumed by index, predates last release):

```markdown
* pkcg01/pkcg02 now return a named list again  <- belongs in the commit + issue, not NEWS
```

## Anti-patterns

- Do not add a NEWS line for a change no user can observe just because you touched
  code.
- Do not document a pre-existing, already-shipped, invisible bug fix.
- Do not create a separate bullet when an existing same-theme entry can absorb it.
- Do not paste implementation detail; describe the effect.
