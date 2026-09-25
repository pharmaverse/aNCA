---
name: nca-scientific-change-review
description: Review aNCA calculation changes for scientific meaning, expected results, and compatibility.
---

# NCA Scientific Change Review

Use for intervals, dosing rules, half-life selection, parameters, units, or
calculation-result changes.

1. State the scientific intent, affected subjects/profiles, parameter(s), units,
   and expected result direction/value.
2. Trace data/mapping through PKNCA inputs, calculation, result shaping, CDISC
   metadata, plots, and export.
3. Compare current and intended behavior with a small representative profile;
   identify unchanged behavior that must remain stable.
4. Define regression evidence and manual scientific review needed.

**Output:** intended meaning, affected paths/results, compatibility risk, and
test scenario. Use `pknca-compatibility` for upstream/version behavior and
`risk-analysis` for material result impact.

Example: “Review a change to half-life exclusion handling before it reaches
PKNCA and exported ADNCA.”
