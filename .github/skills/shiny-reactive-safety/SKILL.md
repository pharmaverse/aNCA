---
name: shiny-reactive-safety
description: Review aNCA Shiny reactive and session state changes for correct user-visible behavior.
---

# Shiny Reactive Safety

Use for reactive expressions, observers, modules, `session$userData`, or
state-dependent UI behavior.

1. Trace the user action through inputs, reactive dependencies, state updates,
   output/rendering, and rerun/reset behavior.
2. Check startup, empty data, changed data, invalid state, repeated action, and
   navigation away/back. Identify state owner and stale-state risk.
3. Prefer one reactive source of truth; avoid hidden observer ordering and
   duplicated session state.
4. Specify a real Shiny workflow: fixture, action, current/expected observable
   result, and focused regression coverage.

**Output:** state/path summary and Shiny verification steps. Use `pr-review`
for evidence-format requirements and `performance-and-large-data-review` when
reactivity can be expensive.

Example: “Review whether a changed mapping invalidates stale exported results.”
