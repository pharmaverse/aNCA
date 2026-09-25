---
name: backward-compatibility-review
description: Assess aNCA change impact on prior settings, outputs, scripts, templates, and callers.
---

# Backward Compatibility Review

Use when changing settings, schemas, arguments, defaults, generated scripts,
templates, or exported artifacts.

1. Identify existing users/consumers and the old contract they rely on.
2. Compare old and new behavior for valid legacy input, optional/removed fields,
defaults, names, types, units, and output locations.
3. Choose compatibility behavior: preserve, safely ignore, translate, warn,
version, migrate, or intentionally break with documented rationale.
4. Verify one legacy scenario and one current scenario; link a follow-up issue
for deferred migration work.

**Output:** affected contract, compatibility decision, migration/fallback,
evidence, and residual risk. Use `shiny-settings-roundtrip`,
`mapping-and-data-contracts`, and `release-version-review` as applicable.

Example: “Review whether an older settings file restores safely when a new
optional grouping variable is absent.”
