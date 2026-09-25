---
name: settings-template-governance
description: Decide whether aNCA behavior belongs in a settings template, metadata, or global code.
---

# Settings Template Governance

Use when adding defaults, mappings, grouping values, or domain-specific
behavior that might otherwise change application-wide code.

1. Identify the affected user/domain and whether behavior varies by template,
   dataset, study type, or all users.
2. Compare existing template/configuration, metadata, and global-code options.
   Prefer the smallest local setting that satisfies the requirement.
3. Define absent/invalid setting and absent-column behavior; do not make a
   global auto-mapping solely to support one template.
4. Verify export/import round-trip and an unaffected template/dataset.

**Output:** chosen layer, rejected alternatives, compatibility behavior, and
focused verification. Use `shiny-settings-roundtrip` and
`mapping-and-data-contracts` for the underlying contract.

Example: “Decide whether preclinical grouping defaults belong in SM/LM templates
or global mapping alternatives.”
