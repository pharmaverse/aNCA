---
name: simplification-first-development
description: >
  Plan or implement an aNCA feature or refactor by finding the smallest
  maintainable change in existing code, settings, or templates before adding
  new code. Use for non-trivial implementation and refactoring decisions.
---

# Simplification-First Development

Use this skill before implementing a non-trivial feature, behavior change, or
refactor. It does not apply to a clearly localized mechanical correction where
the existing path and smallest safe fix are already known.

The goal is to meet the stated acceptance criteria with the fewest independent
code paths and states to maintain and test. Do not turn simplification into an
unrequested rewrite or broaden the product scope.

## Decide Before Editing

1. Trace the affected execution path from its entry point to the user-visible,
   calculated, saved, or exported result. Record the acceptance criterion the
   change must satisfy.
2. Search the relevant existing code, helpers, tests, settings, and templates
   for behavior that can be reused, adjusted, or removed. For Shiny work,
   inspect both `R/` and `inst/shiny/functions/` as required by `AGENTS.md`.
3. Compare the smallest viable options in this order:
   - leave code unchanged and use existing configuration;
   - adjust a settings/template value for the affected domain;
   - simplify, extend, or remove an existing code path;
   - add new code only when the existing paths cannot safely meet the need.
4. Choose the smallest option that fully satisfies the requirement. State why
   the rejected smaller options do not satisfy it; do not add a helper,
   abstraction, or parallel path merely to isolate a one-off expression.
5. Identify the focused verification that protects the changed behavior and
   any behavior removed or consolidated by the simplification.

## Implementation Boundaries

- Prefer one clear source of truth over parallel defaults, mappings, or state.
- Prefer local templates/settings when behavior is domain-specific; do not make
  a global automatic behavior change solely to support one template.
- Reuse or adapt a helper only when its current responsibility remains clear.
  Otherwise, simplify the caller directly rather than creating a thin wrapper.
- Remove obsolete branches, duplicated state, and tests only when the retained
  path covers their intended behavior. Do not retain dead compatibility paths
  without a documented user need.
- Keep the implementation limited to the requested acceptance criteria. Record
  a separately useful improvement as a follow-up issue rather than bundling it.

## Required Output

Before editing, or in the PR description when implementation is requested,
provide a concise decision record:

```markdown
**Path reviewed:** entry point -> affected existing code -> observable result
**Existing candidates:** helpers, settings, templates, or tests considered
**Smallest chosen change:** configuration | existing-path change | new code
**Why:** why smaller alternatives cannot meet the acceptance criterion
**Verification:** focused test or real Shiny workflow, plus any regression risk
```

Example prompt: “Add a preclinical grouping default. Use
`simplification-first-development` to decide whether this belongs in the
template, metadata, or global mapping logic before editing.”

## Related Workflows

- Use `shiny-settings-roundtrip` when the selected change reads, writes, or
  restores settings.
- Use `pknca-compatibility` when the relevant existing behavior depends on
  PKNCA version behavior.
- Use `pr-review` to evaluate whether a proposed PR introduced unnecessary
  complexity or duplicate paths.
- Follow `AGENTS.md` for repository conventions and required code search.
