---
name: shiny-settings-roundtrip
description: >
  Review or implement aNCA Shiny settings export/import and session restore
  changes. Use when work touches YAML settings, restored app state, generated
  session code, mapping, filters, ratio tables, intervals, exclusions, or TLG
  selections.
---

# Shiny Settings Round Trip

Use this skill for changes that affect saving settings, uploading settings, or
replaying an app session from saved configuration. The goal is that a settings
file exported from one session can restore the same meaningful state in another
session, including partial or older settings files where supported.

## Code Paths

Inspect the relevant parts of the flow before editing:

- settings readers and normalization in `R/readers.R`;
- generated script settings handling in `R/get_session_code.R` and
  `inst/www/templates/script_template.R`;
- settings save/export in `inst/shiny/modules/tab_nca/nca_setup.R` and
  `inst/shiny/functions/zip-utils.R`;
- app replay and cross-tab state in `inst/shiny/modules/tab_data.R`,
  `inst/shiny/modules/tab_nca.R`, and related setup modules;
- mapping UI/server code under `inst/shiny/modules/tab_data/`;
- NCA setup modules under `inst/shiny/modules/tab_nca/setup/`;
- tests in `tests/testthat/test-readers.R`,
  `tests/testthat/test-get_session_code.R`, and Shiny app tests under
  `inst/shiny/tests/testthat/`.

Search for existing helpers before adding new ones. Keep settings helpers close
to the domain they support unless they are shared across multiple modules.

## What To Check

For every settings change, consider whether it affects:

- column mapping and skipped/partial mapping state;
- data filters and duplicate-row handling;
- interval definitions, custom intervals, and imputation settings;
- ratio table rows, including imported types and empty optional fields;
- concentration and parameter exclusions;
- slope selector rules;
- TLG order, selected outputs, and sidebar options;
- project name, dataset filename, comments, and settings-version metadata;
- generated R script reproducibility.

Handle older or partial settings defensively. Missing optional sections should
usually normalize to `NULL`, an empty data frame, or existing defaults rather
than breaking restore. Invalid values should be skipped with a clear reason when
that is the existing pattern.

## Implementation Guidance

- Keep serialization and normalization explicit; avoid relying on incidental
  list/data-frame coercion.
- Preserve column names, types, and empty data-frame schemas used by downstream
  modules.
- When a setting is restored asynchronously, check that pending state is cleared
  after successful replay so it does not apply twice.
- If changing settings shape, update reader tests and any fixture YAML files
  needed to cover backward compatibility.
- If the generated R script should reproduce the app behavior, update both the
  app path and script path.

## Verification

Do not install R just to run checks. If R is unavailable, inspect code and CI
logs, and provide manual app checks.

Useful automated checks when R is already available:

- `devtools::test(filter = "readers")`;
- `devtools::test(filter = "get_session_code")`;
- focused Shiny tests under `inst/shiny/tests/testthat/`;
- full CI checks for tests, R CMD check, lint, roxygen, and spellcheck.

Manual app checks should cover:

1. Configure the relevant state in the app.
2. Save/export settings.
3. Restart or reset the app.
4. Upload the saved settings.
5. Confirm mapping, filters, intervals, ratio rows, exclusions, slope rules,
   TLG selections, and comments restore as applicable.
6. Run NCA or generate outputs that depend on the restored state.
7. Export settings again and compare meaningful fields with the original.

## Reporting

Report:

- settings sections affected;
- backward-compatibility behavior for missing or partial fields;
- tests or CI jobs used as evidence;
- manual Shiny checks still needed;
- any user-visible migration or NEWS impact.
