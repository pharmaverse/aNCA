---
name: pr-review
description: Produce evidence-based, actionable pull-request findings for aNCA.
---

# PR Review Evidence Standard

Use this skill when reviewing a pull request. Classify each comment as one of:

- **Issue** — a substantiated defect, regression, missing required behavior, or
  material risk that should be addressed before merge.
- **Question** — information needed to understand the change or decide whether
  an Issue exists.
- **Suggestion** — a concrete, non-blocking improvement.
- **Idea** — an optional future improvement that is outside the PR's required
  scope.
- **Info** — context that needs no author action.

Do not elevate an uncertain observation to an Issue without explaining the
available evidence and labeling its confidence. Do not use a Question merely
to disguise an unsupported Issue.

## Pull-request title

Confirm that the title follows `TYPE: concise outcome (#issue)`: an accurate
uppercase type (`FEATURE`, `FIX`, `REFACTOR`, `SKILL`, `DOCS`, `CI`, or `MAINT`),
a short primary outcome, and the issue number at the end. For example:

```text
FIX: Prevent partial ZIP exports (#1452)
SKILL: Simplification-first development (#1501)
```

Raise a **Suggestion** when the title is unclear or does not follow this
format; do not treat title wording alone as a blocking Issue.

## Required evidence for each substantive Issue

Every substantive Issue must include:

- the relevant file and function, plus the execution path that reaches the
  problem;
- the user or downstream impact;
- a confidence label: **Confirmed** or **Risk**;
- the smallest safe fix;
- evidence-based instructions for testing the fix; and
- a regression-coverage recommendation.

Use a GitHub suggestion block only when the proposed change is localized,
complete, and unambiguous. Do not use a suggestion block for a multi-file
change, an incomplete fix, or a change that needs product or scientific
judgment.

## Testing evidence

For a Shiny Issue, the testing instructions must describe the actual app
workflow, including:

1. the data or fixture state;
2. the user action(s);
3. the current observable behavior; and
4. the expected observable behavior after the fix.

Do not replace an interactive workflow with a function-only test when the
defect is in the app flow. When no app workflow applies, provide the smallest
runnable R or test command that demonstrates the failure or verifies the fix.
Prefer a focused `testthat::test_file()` or `devtools::test(filter = ...)`
invocation over an unrelated full-suite command.

If the diff, tests, or repository do not provide enough evidence to reproduce
the behavior, report the finding as **Risk** and state the missing evidence.
Do not invent a workflow, dataset, current behavior, or reproduction step.

## Issue template

```markdown
### Issue — Short finding title

**Location and path:** `path/to/file.R`, `function_name()` — describe the
execution path that reaches this code.

**Impact:** Describe what a user, exported artifact, or downstream consumer can
observe.

**Confidence:** Confirmed | Risk

**Smallest safe fix:** Describe the minimum change that removes the problem.

**How to test:**
- Shiny workflow: data/fixture state → user action(s) → current observable
  behavior → expected observable behavior; or
- R/test command: the smallest runnable command and expected result.

**Regression coverage:** Name the focused test file and scenario to add or
update.
```

## Completed illustrative example

The following is an illustrative example of a completed Shiny finding. It is
an example of review evidence format, not a claim about a live PR.

```markdown
### Issue — Selected artifacts may be written before export validation finishes

**Location and path:** `inst/shiny/functions/zip-utils.R`,
`prepare_export_files()` → `.validate_outputs_pre_export()` → `save_output()`.
The selected output list is assembled before the pre-export validation gate;
the gate must finish successfully before `save_output()` is reached.

**Impact:** If a selected invalid CDISC output is detected after another
selected output is written, users can receive a partial export rather than an
all-or-nothing ZIP.

**Confidence:** Risk — the PR evidence does not yet show whether a writer can
run before the validation failure.

**Smallest safe fix:** Run the complete selected-output validation before the
first writer call and abort when an error-severity finding is returned.

**How to test:** Start with a valid NCA result table and a selected ADNCA
dataset whose `AVAL` value is character rather than numeric. In the export
dialog, select both outputs and start the ZIP export. Current behavior: not
established by the available evidence; do not claim one. Expected behavior: the
app shows a blocking validation message and produces no ZIP or export files.

**Regression coverage:** Add or update
`tests/testthat/test-zip-utils.R` to select one valid and one invalid output,
expect `Export validation failed`, and assert the export directory is empty.
```

## Checklist before publishing a substantive Issue

- What execution path is wrong?
- What can the user observe?
- What is the smallest safe fix?
- How can the author verify the fix?
- Is the confidence accurately labeled Confirmed or Risk?
- If evidence is incomplete, is the finding labeled Risk without invented
  reproduction details?
- Is a GitHub suggestion block appropriate, localized, complete, and
  unambiguous?
- Does the testing guidance use the real Shiny workflow when the finding is in
  the app?
- Is the regression-coverage recommendation specific?
