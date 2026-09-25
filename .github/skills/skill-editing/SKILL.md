---
name: skill-editing
description: >
  Make a concise, behavior-preserving substantial change to an existing aNCA
  repository skill. Use for skill revisions, not new skills.
---

# Skill Editing

Read the current skill, its linked workflows, and any agent instructions that
refer to it. Use `skill-creation` when available if the request is actually a
new workflow rather than an edit.

## Decide

Ask the requester only for material information that is missing:

- the intended change and its trigger;
- existing behavior that must remain;
- affected related skills/instructions; and
- one representative request, when it would clarify the change.

Infer a short definition of done; ask only if the outcome remains ambiguous.

## Edit

1. Change the smallest relevant instruction section.
2. Preserve the name, description, links, and useful behavior unless the
   request requires changing them.
3. Keep the skill concise: remove stale or duplicated guidance instead of
   appending a parallel workflow.
4. Update a linked instruction only when the changed rule is genuinely
   universal; otherwise keep it in the skill.

## Verify and demonstrate

- Inspect the final skill for scope, broken links, and accidental expansion;
  run the available validator when it can run, otherwise state that it was
  unavailable.
- Run `git diff --check`.
- In the PR description, add a non-mutating **Skill demonstration**: realistic
  prompt, expected decision/output, and any observed result. Do not fabricate a
  run or create temporary commits merely to demonstrate the skill. If safe
  execution is not feasible, state the reviewer check instead.

## Output

Report the preserved behavior, smallest edit, related skills considered,
validation, and PR demonstration. Use the `SKILL` PR type where the repository
title standard is available.
