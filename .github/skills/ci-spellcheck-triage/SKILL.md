---
name: ci-spellcheck-triage
description: >
  Resolve failing Spelling / Spellcheck CI jobs. Use when a PR's spellcheck
  job fails. Reads every flagged word from the CI log, decides per word
  whether to reword the source or add the word to inst/WORDLIST, applies the
  fix to all occurrences, and reports the rationale.
---

# CI Spellcheck Triage

The **Spelling / Spellcheck** job (`insightsengineering/r-spellcheck-action`)
fails when a word appears anywhere in the checked sources that is not in
`inst/WORDLIST`. Each failing word must be resolved either by fixing the text
or by whitelisting the word.

This skill makes the decision repeatable: read the CI result, classify **every**
flagged word, apply the fix to **all** its occurrences, and report what changed.

## When to use

- A PR's `Spelling / Spellcheck` check is red.
- Someone asks "fix the spellcheck failure on PR #NNN".

Do not use this for lint, tests, or roxygen failures — those are different checks.

## Inputs you need

- The PR number (or the workflow run / job id) whose spellcheck job failed.

## Workflow

### 1. Read the failing job log and extract every flagged word

Find the spellcheck job for the PR's latest run and read its log. The action
prints a table like:

```
  WORD  FOUND IN
dev   NEWS.md:9
foo   R/bar.R:42
Number of misspelled words: 2
```

Collect **all** rows under `WORD / FOUND IN` — not just the first. For each,
record the word and every `file:line` location reported. If the log is
truncated, read from the tail so the results table (near the end, before
`Number of misspelled words`) is visible.

Do not stop after one word. The job fails on the first, but you must resolve
the entire list so the next run is green in one pass.

### 2. Confirm all occurrences in the repo

CI reports the location that tripped the check, but the same word may appear in
several files. Before deciding, search the whole repo for each word so the fix
is complete:

```
grep -rn --word-regexp "<word>" R/ inst/ man/ vignettes/ NEWS.md README.md DESCRIPTION 2>/dev/null
```

`man/` is generated — if a word only appears there, fix its **source** roxygen
in `R/` (see "Do not" below), not the generated file.

### 3. Classify each word: reword vs. add to WORDLIST

For every flagged word choose exactly one action.

**Add to `inst/WORDLIST`** when the word is legitimate and intended:

- A domain / PK / CDISC term or parameter code (e.g. `Cmax`, `PPTESTCD`, `AUClast`).
- A package, function, or tool name (e.g. `PKNCA`, `roxygen`, `devtools`).
- A recognised abbreviation or acronym used deliberately (e.g. `dev`, `BLQ`, `IV`).
- A proper noun, unit, or symbol (e.g. `Ĉlast`, `λz`, `µg`).
- A deliberate spelling the project already uses consistently elsewhere.

**Reword the source** when the word is not something we want in the codebase:

- A genuine typo or misspelling (e.g. `existance` -> `existence`).
- A word broken by a stray character, casing, or a bad hyphenation.
- Prose that reads better with a plain-English alternative than with a new
  whitelist entry.

When unsure, prefer **reword** for ordinary English prose and **WORDLIST** for
technical tokens, proper nouns, symbols, and abbreviations. If a word is
genuinely ambiguous, note it in the report and ask rather than guessing.

### 4. Apply the fix for every word

**Reword path — fix all occurrences, not only the CI-reported line:**

- Correct the word in every file found in step 2 (`R/`, `inst/`, `NEWS.md`,
  docs, etc.).
- If it appeared in `man/`, fix the roxygen source in `R/` instead of the
  generated page, and note that `devtools::document()` must be run by a
  developer.
- Keep meaning and surrounding formatting intact.

**WORDLIST path — insert correctly:**

- `inst/WORDLIST` is one word per line, sorted. Insert each new word in its
  correct sorted position.
- Do not add duplicates — check the word is not already present first.
- Preserve exact casing and any special characters/symbols as they appear in
  the source.
- Add one entry per distinct flagged word; do not add variants that were not
  flagged.

Process the **entire** list from step 1 before finishing, so a single follow-up
run clears the job.

### 5. Verify what you can, and report

You cannot run R, so you cannot run `spelling::spell_check_package()` locally —
final verification is the re-run of the spellcheck job in CI. Before handing
off:

- Re-check that each flagged word is either corrected everywhere or present
  once in `inst/WORDLIST`.
- Confirm `inst/WORDLIST` stays sorted with no duplicates.

Then report per word: the word, the action taken (reword or WORDLIST), and a
one-line rationale. Example:

```
- dev     -> WORDLIST (deliberate abbreviation of "development", used in NEWS.md)
- existance -> reword to "existence" in R/foo.R:42 and inst/shiny/bar.R:88 (typo)
```

## Notes and limits

- The check runs on the whole tree, so a failing word may come from the base
  branch, not the PR's own diff. Fixing `inst/WORDLIST` (or the source) still
  resolves it; mention when the cause was inherited from the base branch.
- Adding a word to `inst/WORDLIST` whitelists it repo-wide. Only do this for
  words that are genuinely acceptable everywhere.
- Do not edit generated files under `man/` directly (see `AGENTS.md`).
- Do not disable the check or use `[skip spellcheck]` to get a green run.

## Extending to other checks

The same pattern — **read the CI result -> classify -> apply the fix to all
occurrences -> report rationale** — applies to other LLM-resolvable checks
(e.g. `lintr` style fixes, roxygen/NAMESPACE regeneration prompts). Keep each
such workflow in its own skill so the decision criteria stay explicit.

## References

- `.github/workflows/spellcheck.yml` — the workflow definition.
- `inst/WORDLIST` — the accepted-words list.
- `AGENTS.md` — project conventions (generated files, code principles).
