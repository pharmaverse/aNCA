---
name: pr-contributor-checklist
description: >
  Validate the Contributor checklist for an aNCA pull request. Use when asked
  to complete, verify, or audit PR checklist items without doing slow local
  setup. Prefer remote GitHub Actions results for CI-backed checks and never
  install R just to run checks locally.
---

# PR Contributor Checklist

Use this skill to audit or complete the `Contributor checklist` in an aNCA pull
request. The goal is a quick, evidence-based status for each applicable item:
completed, still failing, waiting on CI, not applicable, or needs human app
testing.

## Hard Rules

- Do not install R, system packages, or large toolchains just to satisfy a
  checklist item. If R/Rscript is absent, say so and use remote CI logs or code
  review instead.
- For CI-backed items, prefer the PR's GitHub Actions checks and job logs over
  local execution.
- Keep the audit timeboxed. If jobs are still running, poll only briefly, then
  report `waiting on CI` and continue with reviewable items.
- Do not mark a checklist item complete just because a local approximation
  passed. Cite the evidence used: CI job, local command, code review, or human
  follow-up needed.

## Inputs

Start from the PR number or PR URL. If the user gives only a branch, identify
the open PR for that branch before auditing checklist status.

Read:

- the PR description and `Contributor checklist`;
- the latest workflow run linked to the PR page;
- the PR mergeability/conflict status;
- the changed files in the PR;
- relevant repo instructions in `AGENTS.md`.

## Checklist Mapping

Use the PR template's checklist items as the source of truth. For each item,
decide how it can be verified.

CI-backed items:

- `Code passes lintr checks` -> inspect the latest `Lint / Lint` job. If it
  fails, use the `ci-lintr-triage` workflow to diagnose and fix.
- `Code passes all unit tests` -> inspect `Tests / Tests` and R CMD check jobs.
  If they fail, read the failing logs and trace the failure to code.
- `New logic covered by unit tests` -> review the diff and test files. CI can
  confirm tests pass, but it cannot prove coverage is meaningful.
- `New logic is documented` -> review changed exported functions, roxygen
  blocks, user-facing docs, and generated artifacts as appropriate. If the
  `Man Pages / Roxygen` job fails, use the `ci-roxygen-triage` workflow.
- `App or package changes are reflected in NEWS` -> review `NEWS.md` against
  user-visible behavior in the diff.
- `Package version is incremented` -> compare `DESCRIPTION` version against
  `origin/main`.
- `R script works with the new implementation` -> inspect generated script code
  paths and relevant tests; use CI if available. If this requires running the
  app or R locally and R is unavailable, mark as human follow-up.
- `Settings upload works with the new implementation` -> inspect settings
  serialization/restore code and tests; use CI if available. If this requires
  interactive app testing, mark as human follow-up.
- `.scss` compile item -> if `.scss` changed, confirm matching
  `inst/shiny/www/main.css` changes are present. Do not run a compiler unless
  the existing environment already supports it.
- `data-raw/test_suggests_hidden.R` item -> if dependencies changed, inspect
  dependency declarations and the `Data Raw / Data Raw` job. Do not install R to
  run this locally.

Spellcheck is not always listed in the template, but it is a required CI check
for aNCA. If it fails, use the `ci-spellcheck-triage` workflow.

## Merge Conflicts

The Contributor checklist does not name merge conflicts directly, but a PR is
not ready while GitHub reports it cannot be merged cleanly. Include mergeability
in the audit when the PR page, branch status, or CI indicates conflicts.

When merge conflicts are present, use the `merge-conflict-resolution` skill for
the detailed resolution workflow. The checklist audit should still track and
report mergeability status alongside CI status.

When conflicts exist:

1. Identify the conflicting files and the base branch involved.
2. Check the origin of both sides before editing: compare the PR diff, current
   `origin/main`, nearby commit history, and related issue/PR descriptions when
   they explain the intended behavior.
3. Resolve by integrating both features where possible. Do not blindly choose
   one side with `ours`/`theirs` unless the surrounding code and issue context
   make that choice clearly correct.
4. After resolving, inspect the final file for semantic consistency, not just
   removal of conflict markers. Update tests, docs, NEWS, or generated artifacts
   when the integrated behavior requires it.
5. If both sides implement incompatible behavior, remove information needed to
   decide, or would require product/domain judgment, stop and ask the person
   what to do. Only proceed without asking when the intended result is obvious
   from the code, PR description, linked issue, or existing tests.

Report merge-conflict status separately from CI status:

- `complete` when the branch is mergeable or conflicts were resolved and pushed;
- `failing` when conflicts remain and the reason is understood;
- `needs human decision` when the conflict is semantically ambiguous.

## Remote CI Workflow

1. Find the latest workflow run for the PR head commit.
2. Read job status for all relevant workflows.
3. For failed jobs, read the tail of the job log first. If the useful failure is
   not visible, page earlier in the log only enough to capture the error.
4. For running jobs, wait briefly only when it is likely to finish soon. A good
   default is one or two short waits, not an open-ended watch.
5. Summarize status by checklist item and include the job name or evidence.

## Local Fallbacks

If R is already available, local commands may be useful for focused checks, but
remote CI remains authoritative for checklist items tied to workflows.

If R is not available:

- Run cheap static checks such as `git diff --check`, file/diff inspection, and
  targeted searches.
- Review roxygen comments, tests, NEWS, version, dependency declarations, and
  generated-file consistency by reading files.
- Clearly report that R-based commands were not run because R was unavailable
  and was intentionally not installed.

## Output

Report a concise checklist audit. For each item include status and evidence:

- `complete` when CI or review supports the item;
- `failing` with the failing job and key log line;
- `waiting on CI` when jobs are still running after the short wait;
- `needs human app test` for interactive Shiny behavior the agent cannot verify;
- `not applicable` when the PR does not touch that area.

End with the smallest next action: fix specific failing jobs, wait for CI, or
ask a human to perform listed app checks.
