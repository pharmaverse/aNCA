---
name: issue-to-pr-scope
description: Keep an aNCA PR traceable to its issue acceptance criteria and evidence.
---

# Issue To PR Scope

Use when opening, reviewing, or finalizing a PR linked to an issue.

1. Read the issue acceptance criteria and classify each as implemented, partly
   implemented, deferred, or not applicable.
2. Link each implemented criterion to changed path(s) and focused evidence:
   unit/integration test, Shiny workflow, SAT, CI, or manual check.
3. Move out-of-scope improvements to a linked follow-up issue; do not imply
   they are delivered by the PR.
4. Ensure the PR title/description states the primary outcome, scope, tests,
   deliberate exclusions, and any human verification still needed.

**Output:** a compact AC-to-implementation-to-evidence table in the PR
description or review. Use `pr-review` for findings and define SAT evidence
when acceptance testing is scripted.

Example: “Map the three IMPROVE integration criteria to this PR’s code and SAT
evidence.”
