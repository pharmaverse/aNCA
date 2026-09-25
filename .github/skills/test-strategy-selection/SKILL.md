---
name: test-strategy-selection
description: Select proportionate aNCA verification evidence for a change or requirement.
---

# Test Strategy Selection

Use before adding tests or defining acceptance evidence for non-trivial change.

1. State the changed behavior, risk, and fastest observable failure.
2. Select the lowest sufficient layer: focused unit, integration, generated
   script, real Shiny workflow, CI evidence, manual check, or scripted SAT.
3. Use more than one layer only when each covers a distinct failure mode.
4. Name fixture, expected value/observable result, and regression scenario.

**Output:** selected evidence, rationale, exact test/workflow, and remaining
human check. Use `risk-analysis` and `test-data-and-fixture-design` as
applicable; define SAT evidence when needed.

Example: “Choose evidence for preventing a partial export after validation
fails.”
