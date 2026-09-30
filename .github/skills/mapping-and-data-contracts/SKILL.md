---
name: mapping-and-data-contracts
description: Define and review aNCA data/mapping contracts for required, optional, and alternative fields.
---

# Mapping And Data Contracts

Use for dataset columns, metadata, mappings, defaults, types, or CDISC-facing
fields. Do not use for a purely visual label change.

1. List required fields, optional fields, accepted alternatives, types, allowed
   values, defaults, and missing-field behavior.
2. Identify the source of truth for each rule (metadata, settings, code, or
   external standard) and the downstream consumer that relies on it.
3. Trace the contract through upload, metadata/mapping, calculation, plotting,
   settings restoration, and export as applicable.
4. Prefer an explicit local contract over a hidden global fallback. Distinguish
   absent, `NULL`, empty, and invalid values.
5. Add focused fixtures for valid, optional-absent, and invalid cases.

**Output:** contract table including source of truth and downstream consumer,
plus affected paths and test cases. Link
`shiny-settings-roundtrip`, `settings-template-governance`, and
`backward-compatibility-review` when applicable.

Example: “Define behavior when a preclinical settings template names GENDER but
the uploaded dataset has no GENDER column.”
