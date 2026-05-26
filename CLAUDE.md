# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

aNCA is an R package providing a Shiny app for automated Non-Compartmental Analysis (NCA) on pre-clinical and clinical pharmacokinetic data. It builds on `PKNCA` (>= 0.12.1) as the core NCA engine and produces CDISC outputs (ADNCA, PP, ADPP), configurable TLGs, and slide decks.

**Read `AGENTS.md`** — it is the single source of truth for all conventions, code principles, and anti-patterns. The sections below only add what AGENTS.md doesn't already cover.

## Build and test commands

```r
devtools::load_all()        # Load package in development
devtools::document()        # Rebuild NAMESPACE + man/ after roxygen changes
devtools::test()            # Run all unit tests
devtools::test_active_file()# Run current test file only
devtools::check()           # Full R CMD check
lintr::lint_package()       # Code style check
styler::style_file("R/foo.R") # Format a file to tidyverse style
```

To launch the Shiny app: `aNCA::run_app()` — automatically loads test data on startup. Set `aNCA_LOG_LEVEL="TRACE"` in `.Renviron` for debug logging.

CI runs on every PR via GitHub Actions: lintr, spelling, R CMD check, test coverage, and documentation validation.

## Two code zones

**`R/`** — Package API. Functions usable without the Shiny app. Exported functions live here with full roxygen2 docs. No `library()`/`require()` allowed. Imports used via `@importFrom`, Suggests used via `pkg::fun()`.

**`inst/shiny/`** — Shiny application. `app.R` is the entry point, `modules/` contains the tab-level UI+server modules, `functions/` holds app-only helpers (auto-sourced by app.R). Code here can use `require()` since it runs inside the Shiny process.

## Shiny data flow

```
data_upload_server → adnca_raw (reactive) → tab_data_server → tab_nca_server
                                                                    ↓
                                              processed_pknca_data → tab_tlg_server
```

Module communication uses `session$userData`:
- `session$userData$results` — CDISC datasets and NCA results
- `session$userData$project_name()` — reactive, auto-populated from STUDYID
- `session$userData$slope_rules()` — reactive
- `session$userData$settings_versions` — versioned YAML settings list

## Tab structure

| Tab | Module file | Purpose |
|-----|------------|---------|
| Data | `tab_data.R` + `tab_data/` | Upload, mapping, filtering, data preprocessing |
| Exploration | `tab_explore.R` + `tab_explore/` | Interactive plots (line, boxplot, PK/Dose QC) |
| NCA | `tab_nca.R` + `tab_nca/` | NCA settings, parameter selection, slope selector, ratios, exclusions, results |
| TLG | `tab_tlg.R` + `tab_tlg/` | Tables/listings/graphs defined in `inst/shiny/tlg.yaml` |
| About | `tab_about.R` | Citations, session info, app version |

## Exports and ZIP

Export workflow in `tab_nca/zip.R`: `TREE_LIST` defines export structure. Selected items come from `input$res_tree`. ZIP filename uses project name or falls back to `NCA_STUDYID.zip`. Pre-Specs sheets are auto-generated from CDISC data.

## Styling (CSS/SCSS)

Styles authored in `inst/shiny/www/styles/` SCSS partials, compiled to `main.css`. Entry point: `styles/main.scss`. When editing styles, modify the `.scss` partial **and** apply the same change to `main.css` (the R compile script may not be available). Sidebar width is `--_sidebar-width: 170px !important` in `main.css`.

## Exploration plots conventions

- Default `color_by` priority: `DOSEA > TRT01A > GROUP > ACTARM > COHORT`
- Default `facet_by` priority: `TRT01A > DOSEA > GROUP > ACTARM > COHORT`
- Auto-mapping includes: `COHORT`, `PART`, `PERIOD`

## Color constants

- **SCSS variables**: `inst/shiny/www/styles/modules/_colors.scss`
- **R constants**: `inst/shiny/functions/colors.R` (auto-sourced, available in all Shiny modules)
- **Exclusion colors**: `inst/shiny/functions/utils-exclusions.R` — `EXCL_COLOR_NCA`, `EXCL_COLOR_TLG`, `EXCL_COLOR_BOTH`, `EXCL_COLOR_PARAM`
- **Flag colors**: `inst/shiny/modules/tab_nca/nca_results.R` — `FLAG_COLOR_FLAGGED`, `FLAG_COLOR_MISSING`

## Imputation pipeline

BLQ (Below Limit of Quantification) and start concentration (C0) imputation spans multiple files:

| Step | File | Key function |
|------|------|-------------|
| UI | `inst/shiny/modules/tab_nca/setup/data_imputation.R` | `data_imputation_server()` — returns `blq_imputation_rule`, `impute_c0`, `blq_strategy` |
| Settings assembly | `inst/shiny/modules/tab_nca/setup/settings.R#452-491` | `settings()` reactive — bundles `data_imputation` into the settings list |
| Data object update | `R/PKNCA.R#297` | `PKNCA_update_data_object(start_impute, blq_imputation_rule)` |
| Interval imputation | `R/intervals.R#215` | `update_main_intervals(impute, blq_imputation_rule)` — sets `data$intervals$impute` column |
| Start impute | `R/create_start_impute.R` | `create_start_impute()` — adds C0 strategies (`start_conc0`, `start_predose`, `start_logslope`, `start_c1`) |
| BLQ global function | `R/PKNCA.R#439` | `PKNCA_calculate_nca(blq_rule)` — registers `PKNCA_impute_method_blq` globally before calling `PKNCA::pk.nca()` |
| Obs param cleanup | `R/intervals.R#326` | `rm_impute_obs_params()` — removes imputation from non-AUC-dependent parameters |

### Critical guard

`update_main_intervals()` must ensure `data$intervals$impute` exists **before** the BLQ `dplyr::mutate()` block. If the column is missing and `impute` is also a function parameter (e.g. `FALSE` from YAML `impute_c0: no`), dplyr resolves to the parameter, producing `"blq, FALSE"` which PKNCA interprets as a lookup for `PKNCA_impute_method_FALSE`. The guard `if (!"impute" %in% names(data$intervals))` (see `rm_impute_obs_params()` line 341) prevents this.

## Interval parameter helpers

- **`rename_interval_params()`** (`R/utils.R:17`): Appends `_start-end` suffix to PPTESTCD for manual interval params (e.g. `AUCINT` → `AUCINT_0-24`). Called in `descriptive_statistics.R`, `parameter_plots.R`, `flexible_violinboxplot.R`.
- **`parse_interval_parameter()`** (`R/ratio_calculations.R:326`): Reverses the above — extracts `base`, `start`, `end`, `is_interval` from a suffixed name. Used when labels need to be resolved from `metadata_nca_parameters` (which only stores base PPTESTCDs).

## YAML boolean caveat

R's `yaml` package parses YAML 1.1 booleans: `yes`/`no`/`true`/`false`/`on`/`off` all become R logical values. When settings YAML values like `impute_c0: no` are parsed, they become logical `FALSE`, not the string `"no"`. Code that passes these values into `if()` conditions or dplyr expressions must handle logical values correctly — prefer `isTRUE()` / `isFALSE()` guards over bare `if (value)` when the value may be logical from YAML.

## Testing conventions

- Test files: `tests/testthat/test-<source>.R`, matching source file name
- Use `describe()` / `it()` blocks with value-level assertions
- Test data created in `tests/testthat/setup.R` and `tests/testthat/data/`
- E2E tests in `test-e2e-runner.R` use `shinytest2` (in Suggests)

## Package versioning and release

- Semantic versioning. Bump `DESCRIPTION` version (+1 from main) for non-trivial changes
- `{admiral}` family follows quarterly releases (Mar/Jun/Sep/Dec); aNCA extensions should align within 2 weeks
- `NEWS.md` entries reference PR numbers

## PR workflow summary

1. `git fetch origin main && git checkout origin/main` — branch from latest upstream
2. Branch name: `<issue-number>-<type>/<short-description>`
3. After code changes: `document()` → `lint_package()` → `test()` → `check()`
4. Update `NEWS.md`, bump `DESCRIPTION` version
5. Push to fork, create PR against `pharmaverse/main`
6. Add 2+ reviewers from DESCRIPTION authors
7. Never force-push; use separate commits instead of amending
