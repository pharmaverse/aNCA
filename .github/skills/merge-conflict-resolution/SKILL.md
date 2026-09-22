---
name: merge-conflict-resolution
description: >
  Resolve or assess merge conflicts in aNCA pull requests. Use when a PR is not
  mergeable, a branch needs to be updated from main, or conflict markers appear.
  Understand both sides before editing and ask when the intended integration is
  ambiguous.
---

# Merge Conflict Resolution

Use this skill when a PR cannot merge cleanly or conflict markers appear in the
working tree. The goal is to integrate the PR's intent with the target branch,
not merely to remove conflict markers.

## Principles

- Preserve both features whenever they can coexist.
- Do not blindly choose `ours` or `theirs`. Those are acceptable only when the
  surrounding code, linked issue, PR description, or tests make the correct side
  obvious.
- Treat conflicts in app flows, exports, data transformations, settings, and
  tests as semantic until proven mechanical.
- Ask the person what to do when the resolution would drop behavior, change user
  workflow, alter scientific/CDISC output, or require product/domain judgment.
- Keep unrelated refactors out of the conflict-resolution commit.

## Workflow

1. Identify the PR head branch, base branch, and conflicting files from GitHub
   or from local Git conflict markers.
2. Read the PR description and linked issues for the head branch intent.
3. Inspect the base-side change that caused the conflict:
   - compare with `origin/main`;
   - read nearby commit messages or merged PRs touching the same file;
   - inspect tests/docs/NEWS added by both sides when available.
4. Classify each conflict:
   - `mechanical`: formatting, nearby independent additions, regenerated order;
   - `semantic`: both sides changed the same behavior or data contract;
   - `ambiguous`: intent cannot be inferred safely.
5. Resolve mechanical conflicts directly.
6. Resolve semantic conflicts only after integrating both behaviors deliberately.
   Update nearby tests, docs, NEWS, or generated artifacts if the integration
   changes expected behavior.
7. Stop and ask for guidance on ambiguous conflicts. State the competing intents
   and the concrete choices.

## Verification

After editing:

- search for conflict markers with `grep -RIn '<<<<<<<\\|=======\\|>>>>>>>'`;
- run `git diff --check`;
- inspect the final hunks to ensure both sides' intended behavior is represented;
- run available focused checks. Do not install R if it is missing; use CI or
  code review fallback instead;
- check whether docs, NEWS, tests, settings restore, or generated outputs need
  follow-up updates.

## Reporting

Report:

- conflicting files and whether each was mechanical, semantic, or ambiguous;
- what intent each side carried;
- how the final resolution preserves or intentionally changes behavior;
- checks run locally and checks left to CI/human review;
- any human decision still needed.

When this work is part of a PR checklist audit, report mergeability separately
from CI status.
