---
name: skill-creation
description: >
  Create a concise, repository-owned aNCA skill when a recurring,
  decision-heavy workflow needs reusable guidance. Use for new skills, not
  substantial edits to an existing skill.
---

# Skill Creation

Use this workflow only when an existing skill or agent instruction cannot cover
the repeated task without becoming unclear or overly broad. Prefer extending an
existing skill for a small, closely related workflow.

## Decide

Read the relevant existing skills and instructions first. Ask the requester
only for material information that is missing:

- the trigger/use case;
- the intended outcome or evidence;
- the boundary with existing skills;
- required related links; and
- one representative request, when it would clarify the workflow.

Infer a short definition of done from the request. Ask about it only if the
desired result is still ambiguous.

## Create

1. Choose a short, distinct hyphenated name.
2. Add only a `SKILL.md` unless a script, reference, or asset changes real
   decisions or avoids repeated work.
3. Write a discriminating description, trigger, minimum workflow, required
   output/evidence, and links to related repository skills. Do not copy their
   instructions.
4. Keep universal rules in agent instructions only when they genuinely apply to
   all work; otherwise keep them in the skill.

## Verify and demonstrate

- Inspect the new skill for scope, overlap, and usable links; run the available
  skill validator when it can run, otherwise state that it was unavailable.
- Run `git diff --check`.
- In the PR description, add a non-mutating **Skill demonstration**: a realistic
  prompt, expected decision/output, and any observed result. Do not fabricate a
  run or create temporary commits merely to demonstrate the skill. If safe
  execution is not feasible, state the reviewer check instead.

## Output

Report the chosen scope, related skills considered, files changed, validation,
and the PR demonstration. Use the `SKILL` PR type where the repository title
standard is available.

For substantial changes to an existing skill, use `skill-editing` when it is
available; the final skill-integration PR will complete cross-links.
