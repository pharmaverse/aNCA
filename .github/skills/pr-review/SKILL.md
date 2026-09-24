---
name: pr-review
description: Produce evidence-based, actionable pull-request findings for aNCA.
---

# PR Review Evidence Standard

Use this skill when reviewing a pull request. Apply it to every substantive
**Error** or **Warning** finding. Do not elevate an uncertain observation to an
Error or Warning without explaining the evidence and labeling its confidence.

## Required evidence for each Error or Warning

Every substantive finding must include:

- the relevant file and function, plus the execution path that reaches the
  problem;
- the user or downstream impact;
- a confidence label: **Confirmed** or **Risk**;
- the smallest safe fix;
- evidence-based instructions for testing the fix;
- a regression-coverage recommendation.

Use a GitHub suggestion block only when the proposed change is localized,
complete, and unambiguous. Do not use a suggestion block for a multi-file
change, an incomplete fix, or a change that needs product or scientific
judgment.

For a Shiny finding, the test instructions must describe the actual app
workflow: the starting state, user inputs/actions, and observable expected
result. Do not replace this with a function-only test when the defect is in an
interactive flow.

When no app workflow applies, provide the smallest runnable R or test command
that demonstrates the failure or verifies the fix. Prefer a focused
`testthat::test_file()` or `devtools::test(filter = ...)` invocation over an
unrelated full-suite command.

## Finding template

```markdown
### [Error|Warning] Short finding title

**Location and path:** `path/to/file.R`, `function_name()` — describe the
execution path that reaches this code.

**Impact:** Describe what a user, exported artifact, or downstream consumer can
observe.

**Confidence:** Confirmed | Risk

**Smallest safe fix:** Describe the minimum change that removes the problem.

**How to test:**
- Shiny workflow: starting state → user action(s) → expected observable result; or
- R/test command: the smallest runnable command and expected result.

**Regression coverage:** Name the focused test file and scenario to add or
update.
```

## Checklist before publishing a substantive finding

- What execution path is wrong?
- What can the user observe?
- What is the smallest safe fix?
- How can the author verify the fix?
- Is the confidence accurately labeled Confirmed or Risk?
- Is a GitHub suggestion block appropriate, localized, complete, and
  unambiguous?
- Does the testing guidance use the real Shiny workflow when the finding is in
  the app?
- Is the regression-coverage recommendation specific?
