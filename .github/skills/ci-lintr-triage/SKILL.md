---
name: ci-lintr-triage
description: >
  Resolve failing Lint / Lint CI jobs. Use when a PR's lintr check fails.
  Reads each lint from the CI log (linter, file, line, message), consults
  the project's .lintr config for the expected standards, and fixes the
  affected code to satisfy the linter.
---

# CI Lintr Triage

The **Lint / Lint** job runs `lintr::lint_package()` and fails when any lint is
reported. Each lint names the offending file, line, and the specific linter that
fired. The expected standards come from the repo's `.lintr` config, not from
lintr's defaults alone.

This skill makes the fix repeatable: read the CI result, look up what the
relevant linter expects in `.lintr`, and correct the affected files so the code
conforms — without weakening the config.

## When to use

- A PR's `Lint / Lint` check is red.
- Someone asks "fix the lint failure on PR #NNN".

Do not use this for spellcheck, tests, or roxygen failures — those are different
checks (see `ci-spellcheck-triage` for spelling).

## Inputs you need

- The PR number (or the workflow run / job id) whose lint job failed.

## Workflow

### 1. Read the failing job log and extract every lint

Find the lint job for the PR's latest run and read its log. `lintr` prints each
lint as a structured entry, for example:

```
[[1]]
$filename      "tests/testthat/test-detect_study_types.R"
$line_number   179
$column_number 7
$type          "style"
$message       "Put a space before `%>%` and a new line after it, unless the full pipeline fits on one line."
$line          "    ) %>% select(-VOL)"
$linter        "pipe_continuation_linter"
```

Collect **every** entry — `lintr` can report many at once. For each, record:
`filename`, `line_number`, `column_number`, the `linter` name, and the `message`.
If the log is long, read from the tail so the printed lints (just before the
`stop("Lints detected...")`) are visible.

Do not stop after one lint. Resolve the full list so the next run is green in a
single pass.

### 2. Consult `.lintr` for the expected standard

**Always read the repo's `.lintr` — it is the source of truth.** The project
customises several linters, so lintr's defaults do not apply, and any thresholds
(line length, cyclomatic complexity, etc.) must be taken from `.lintr` at the
time you run, not assumed. Map each failing `linter` name to its configured rule
there.

Some facts about this config are non-obvious from a lint message and easy to get
wrong, so keep them in mind (but still confirm against `.lintr`):

- The pipe style is enforced — use the magrittr `%>%` pipe, not `|>`.
- Assignment style is enforced — assign with `<-`/`<<-`, not `=`.
- Return style is implicit — do not add an explicit final `return(...)`.
- Object names must follow the configured case styles (snake_case / SNAKE_CASE).
- `object_usage_linter` is **disabled** — do not chase usage lints.
- `exclusions` lists paths excluded from linting entirely — do **not** "fix"
  lints in excluded files.

For any rule with a numeric threshold (e.g. line length, cyclocomp), read the
exact value from `.lintr` rather than relying on memory — these are the most
likely to change.

### 3. Fix the affected code to satisfy the linter

For each lint, edit the named file at the named line so it conforms to the
standard from step 2, keeping behaviour identical. Typical fixes:

- **`line_length_linter`** — break the line under the configured limit (wrap
  arguments, a pipe step, or a long string) without changing logic.
- **`pipe_continuation_linter`** — put each pipe step on its own line with a
  newline after `%>%` when the pipeline spans multiple lines, e.g.
  `bind_rows(...) %>%` then `  select(-VOL)` on the next line.
- **`pipe_consistency_linter`** — replace `|>` with `%>%`.
- **`assignment_linter`** — replace `=` used for assignment with `<-`.
- **`object_name_linter`** — rename to snake_case / SNAKE_CASE, updating **all**
  references to the renamed object.
- **`return_linter`** — remove a trailing `return(...)`, leaving the bare
  expression as the last line.
- **`seq_linter`** — replace `1:length(x)` with `seq_along(x)` and `1:n` with
  `seq_len(n)`.
- **`cyclocomp_linter`** — the function exceeds the configured complexity limit;
  reduce branching by extracting a helper or simplifying control flow. This is a
  real refactor, so keep it behaviour-preserving and add/adjust tests if logic
  moves.

Fix the code, not the config: do not raise limits, disable linters, or add
paths to `exclusions` to make a genuine lint pass. Changing `.lintr` also has a
side effect — when `.lintr` is in a PR's changed files, the CI lints the **whole
package**, not just the diff.

Never use `[skip lint]` to get a green run.

### 4. Verify what you can, and report

You cannot run R, so you cannot run `lintr::lint_package()` locally — final
verification is the re-run of the lint job in CI. Before handing off:

- Re-check each reported `file:line` now conforms to the rule from `.lintr`.
- For renames, confirm every reference was updated.
- Watch for fixes that introduce a *new* lint (e.g. a rewrap that pushes another
  line over the length limit).

Then report per lint: file:line, the linter, and the fix applied. Example:

```
- test-detect_study_types.R:179 pipe_continuation_linter -> put select(-VOL) on its own line after %>%
- R/foo.R:42 line_length_linter -> wrapped the call arguments to stay under the limit
```

## Notes and limits

- A lint may come from the base branch rather than the PR's diff (especially if
  `.lintr` changed, which triggers whole-package linting). Note when the cause
  is inherited.
- Do not edit generated files under `man/` (see `AGENTS.md`); fix roxygen source
  in `R/` instead.
- Prefer the smallest change that satisfies the linter while keeping the code
  readable and behaviour unchanged.

## References

- `.github/workflows/lintr.yml` — the workflow definition.
- `.lintr` — the linter configuration (the source of truth for standards).
- `AGENTS.md` — project conventions (code principles, generated files).
