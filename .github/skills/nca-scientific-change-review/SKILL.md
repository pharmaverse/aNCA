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
   metadata, plots, and export. For submission-facing outputs, identify the
   applicable CDISC standard/version or metadata authority, then check the
   dataset structure, variable definitions, controlled terminology, and
   traceability through to tables, listings, and figures (TLGs). Treat CDISC
   dataset compliance and TLG conventions as related but distinct checks;
   verify the relationships between those objects as well: each analysis
   dataset (for example, ADPP or ADNCA) must contain sufficient data and
   metadata to explain the analysis performed with the NCA settings and to
   produce its corresponding TLGs, and each TLG must have an explicit source
   dataset and derivation path. Record any deliberate deviation or unresolved
   compliance risk.
3. Compare current and intended behavior with a small representative profile;
   identify unchanged behavior that must remain stable.
4. Define regression evidence and manual scientific review needed.

**Output:** intended meaning, affected paths/results, CDISC object
relationships and submission-output impact, compatibility risk, and test
scenario. Use `mapping-and-data-contracts` for field definitions and
missing-field behavior, `pknca-compatibility` for upstream/version behavior,
and `risk-analysis` for material result impact.

Example: “Review a change to half-life exclusion handling before it reaches
PKNCA and exported ADNCA.”
