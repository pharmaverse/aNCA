---
name: issue-discovery-and-authoring
description: Create an evidence-based aNCA issue for a defect, requirement gap, validation concern, or focused improvement.
---

# Issue Discovery And Authoring

Use when an observation needs a tracked decision or change. Do not create an
issue for an already-resolved question or an unsupported guess.

1. Inspect the relevant path, tests, issues, and PRs. Separate observed facts
   from risks and state missing evidence.
2. Classify the work: defect, incomplete requirement, validation gap, refactor,
   or enhancement. Search for duplicates and link related work.
3. Use the repository issue template. State impact, affected path, acceptance
   criteria, evidence needed, and any validation/SAT implication.
4. Split independently reviewable work into linked follow-up issues; do not
   hide unrelated improvements in one issue.

**Output:** issue link, classification, evidence, acceptance checklist, and
related issues/PRs. Use `risk-analysis` for material calculation, record,
export, settings, or integration risk.

Example: “Turn this suspected partial-export behavior into an issue; label it
Risk unless the writer path is demonstrated.”
