---
name: issue-to-pr-scope
description: Keep an aNCA PR traceable to its issue acceptance criteria and evidence.
---

# Issue To PR Scope

Use when opening, reviewing, or finalizing a PR linked to an issue.

1. Read the issue acceptance criteria and classify each as implemented, partly
   implemented, deferred, or not applicable.
2. Link each implemented criterion to changed path(s) and focused evidence:
   unit/integration test, Shiny workflow, CI, code inspection, or manual check.
3. Move out-of-scope improvements to a linked follow-up issue; do not imply
   they are delivered by the PR.
4. Ensure the PR description states scope, tests, deliberate exclusions, and
   any human verification still needed. Use the `Create a PR` section of
   `agent-workflow` for the title and `pr-review` to review it; this workflow
   does not redefine title rules.

**Output:** put this table in the PR description (or review when the
description cannot be edited):

```markdown
## Acceptance criteria and evidence

| Requirement | Status | Proof method | Evidence/result | Follow-up |
|---|---|---|---|---|
| AC1: ... | Complete / Partial / Deferred / N/A | App workflow / test / R script / CI / code inspection / manual check | Link, command, result, or observation | None or issue link |
```

Complete the table with this checklist:

- [ ] Every issue requirement is listed.
- [ ] Every requirement has a status.
- [ ] Every completed requirement names how it was proven.
- [ ] App requirements include the user action and expected result.
- [ ] Test, script, or CI evidence names the relevant file, command, or check.
- [ ] Manual checks state exactly what the reviewer must observe.
- [ ] Partial or deferred requirements explain what remains.
- [ ] Out-of-scope work links to a follow-up issue.
- [ ] No evidence field is blank.

Use `issue-discovery-and-authoring` for issue acceptance criteria,
`test-strategy-selection` for choosing proportionate proof, and
`pr-contributor-checklist` for the repository's standard CI checklist. Use
formal SAT terminology only when the requirement specifically calls for it.

Example: “Map the three integration criteria to this PR's code and state how
each one is proven.”
