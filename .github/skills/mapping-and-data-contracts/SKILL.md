---
name: mapping-and-data-contracts
description: Define and review aNCA data/mapping contracts for required, optional, and alternative fields.
---

# Mapping And Data Contracts

Use for dataset columns, metadata, mappings, defaults, types, or CDISC-facing
fields. Do not use for a purely visual label change.

1. List required fields, optional fields, accepted alternatives, types, allowed
   values, defaults, and missing-field behavior.
2. Trace the contract through upload, metadata/mapping, calculation, plotting,
   settings restoration, and export as applicable.
3. Prefer an explicit local contract over a hidden global fallback. Distinguish
   absent, `NULL`, empty, and invalid values.
4. Add focused fixtures for valid, optional-absent, and invalid cases.

**Output:** contract table plus affected paths and test cases. Link
`shiny-settings-roundtrip`, `settings-template-governance`, and
`backward-compatibility-review` when applicable.

Example: “Define behavior when a preclinical settings template names GENDER but
the uploaded dataset has no GENDER column.”
