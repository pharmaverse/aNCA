---
name: ci-roxygen-triage
description: >
  Resolve failing Roxygen / Man Pages CI jobs. Use when a PR's roxygen check
  fails because man/ or DESCRIPTION are out of date. Reads the CI diff, traces
  each stale man page back to its roxygen source in R/, and fixes the source so
  regeneration would produce the committed output.
---

# CI Roxygen Triage

The **Roxygen** job (workflow "Man Pages") runs
`roxygen2::roxygenize('.', roclets = c('rd', 'collate', 'namespace'))` and then
fails if `man/*` or `DESCRIPTION` differ from what is committed. A failure means
the generated docs / `NAMESPACE` / collate are out of sync with the roxygen
comments in `R/`.

The fix is always in the **source** (`R/*.R` roxygen blocks, and the manually
maintained `NAMESPACE`/`DESCRIPTION` entries the project keeps in step), not in
the generated `man/` files.

## When to use

- A PR's `Roxygen` / `Man Pages` check is red.
- Someone asks "fix the roxygen / man pages failure on PR #NNN".

Do not use this for spellcheck, lint, or test failures — those are different
checks.

## Inputs you need

- The PR number (or the workflow run / job id) whose roxygen job failed.

## Workflow

### 1. Read the failing job log and the reported diff

Find the roxygen job for the PR's latest run and read its log. On failure it
prints the offending diff, for example:

```
Manuals are not up-to-date with roxygen comments!
The following differences were noted:
diff --git a/man/positive_mean.Rd b/man/positive_mean.Rd
...
roxygen2 version that was used in this workflow: 7.3.3
Please ensure that the 'RoxygenNote' field in the DESCRIPTION file matches this version
```

Record **every** file in the diff — the check regenerates the whole package, so
multiple `man/*.Rd`, `NAMESPACE`, and/or `DESCRIPTION` may all be listed. Also
note the **roxygen2 version** the workflow used; a mismatch with `RoxygenNote`
in `DESCRIPTION` can itself cause noise.

### 2. Trace each stale artifact to its source

Do not edit files under `man/` — they are generated (see `AGENTS.md`). Map each
diff back to the source that produces it:

- **`man/<fn>.Rd`** -> the roxygen block above `<fn>` in `R/<fn>.R`. The diff
  shows what regeneration *would* produce; the committed `.Rd` is stale because
  the roxygen block was changed (or should have been) and not regenerated, or
  was edited inconsistently.
- **`NAMESPACE`** -> `@export` / `@importFrom` / `@import` tags in the relevant
  `R/*.R` roxygen blocks. Per `AGENTS.md`, when `@importFrom pkg fun` is
  added/changed the matching `importFrom(pkg, fun)` line in `NAMESPACE` must be
  updated too, and the package must be in `DESCRIPTION` Imports.
- **`DESCRIPTION`** (`Collate:` field) -> file renames/additions or `@include`
  tags; the collate order is regenerated from these.
- **`DESCRIPTION`** (`RoxygenNote:`) -> must match the roxygen2 version the
  workflow reported in step 1.

### 3. Fix the source so regeneration matches the commit

For each artifact, correct the **source** so that running roxygenize would
produce exactly what the diff shows should be there:

- **Content diffs in `.Rd`** (title, `@param`, `@return`, `@examples`,
  `@details`) -> edit the corresponding roxygen tags in `R/*.R` so they match.
  Common causes: a new/renamed function argument without a matching `@param`, a
  changed return value without an updated `@return`, or a description edited in
  the `.Rd` by hand instead of in the source.
- **Export/import diffs in `NAMESPACE`** -> add/remove the `@export` or
  `@importFrom` tag in the roxygen block, then apply the matching change to
  `NAMESPACE` by hand (the project maintains these together, since the agent
  cannot run `devtools::document()`). Confirm any imported package is in
  `DESCRIPTION` Imports.
- **`Collate` diffs** -> add the missing `@include`, or update the field to the
  regenerated order.
- **`RoxygenNote` mismatch** -> set `RoxygenNote` in `DESCRIPTION` to the version
  the workflow reported. Do not fabricate a version; use the one from the log.

Follow the project's roxygen conventions in `AGENTS.md` (exported functions need
`@param`, `@return`, `@export`; keep `zzz.R` globals sorted; etc.).

### 4. Verify what you can, and report

You cannot run R, so you cannot run
`roxygen2::roxygenize()` / `devtools::document()` locally — the generated `man/`
and `NAMESPACE` cannot be produced here, so **the regenerated files themselves
must be created by a developer running `devtools::document()`**, and final
verification is the CI re-run.

Before handing off:

- Confirm each source change (roxygen tags, `NAMESPACE` lines, `DESCRIPTION`
  fields) is consistent with what the diff said the output should be.
- Check `NAMESPACE` and `DESCRIPTION` Imports agree for every `@importFrom`.
- Flag clearly that `devtools::document()` must be run and the regenerated
  `man/*`/`NAMESPACE` committed — the source edits alone will not turn the check
  green unless the generated files are also regenerated and pushed.

Then report per artifact: the file, the source it traces to, and the fix
applied. Example:

```
- man/positive_mean.Rd -> R/positive_mean.R: added @param na.rm (new argument)
- NAMESPACE -> R/foo.R: added @importFrom dplyr filter + importFrom(dplyr,filter)
- DESCRIPTION RoxygenNote -> set to 7.3.3 to match the workflow
- ACTION REQUIRED: run devtools::document() and commit man/ + NAMESPACE
```

## Notes and limits

- The check regenerates the whole package, so a stale artifact can originate
  from the base branch rather than the PR's diff. Note when the cause is
  inherited.
- Never edit `man/` or `NAMESPACE`/`DESCRIPTION` generated content to
  "match" without fixing the roxygen source — the next regeneration would just
  revert it.
- Do not use `[skip lint]` (which also skips this job) to get a green run.
- The agent's source edits are necessary but **not sufficient**: a developer
  must run `devtools::document()` to produce the committed generated files.

## References

- `.github/workflows/man-pages.yml` — the workflow definition.
- `AGENTS.md` — roxygen conventions, generated-file rules, NAMESPACE handling.
- `DESCRIPTION` — Imports, Collate, RoxygenNote.
