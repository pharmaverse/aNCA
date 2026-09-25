---
name: risk-analysis
description: Assess material aNCA change risk and select proportionate controls and verification.
---

# Risk Analysis

Use before changes to calculations, mappings, settings, CDISC/export data,
electronic records, or IMPROVE integration. Do not use for clearly cosmetic
changes with no behavioral effect.

1. State the intended behavior and affected record, calculation, or workflow.
2. Identify credible failure modes, cause, user/downstream impact, and existing
   controls. Distinguish Confirmed evidence from Risk.
3. Rate severity, likelihood, and detectability as low/medium/high; explain the
   rating in one sentence rather than inventing precision.
4. Choose the smallest control and verification: focused test, integration
   test, Shiny workflow, SAT, or manual evidence. Record residual risk and any
   follow-up issue.

**Output:** compact risk table or bullets: failure, impact, rating, control,
verification, residual risk. Link `pr-review` for evidence reporting and
define SAT evidence when scripted acceptance testing is needed.

Example: “Assess risk of restoring a setting whose optional mapped column is
absent from the uploaded data.”
