---
name: pknca-compatibility
description: >
  Investigate or implement aNCA changes affected by PKNCA version compatibility.
  Use when work touches PKNCAdata/PKNCAresults, interval columns, parameter
  registration, half-life slope selection, or PKNCA CRAN vs development behavior.
---

# PKNCA Compatibility

Use this skill for changes that may behave differently across PKNCA versions.
The goal is to keep aNCA compatible with the current supported PKNCA release
while preparing for relevant upstream development changes.

## Upstream Source

When behavior is uncertain or version-specific, inspect PKNCA upstream rather
than guessing:

- GitHub repository: `https://github.com/humanpred/pknca`
- Compare the installed/declared version in `DESCRIPTION` with upstream `main`
  or the branch referenced by the issue/PR.
- Prefer source code, NEWS, tests, and linked PKNCA issues/PRs over assumptions.

Do not install PKNCA or R just to investigate if the environment does not
already support it. Use repository source review and CI logs instead.

## High-Risk Areas

Pay special attention to:

- `PKNCA_create_data_object()` and `PKNCA_update_data_object()`;
- `PKNCA::PKNCAdata()`, `PKNCAconc()`, `PKNCAdose()`, and `PKNCA::pk.nca()`;
- concentration/dose column mappings in `data$conc$columns` and
  `data$dose$columns`;
- interval columns, `start`/`end`, dose-relative interval fields, and imputation
  columns;
- custom or fallback parameter registration in `R/PKNCA_extra_parameters.R`;
- half-life slope selector fields: `include_half.life`, `exclude_half.life`,
  `is.included.hl`, `is.excluded.hl`, `REASON`, `LAMZMTD`, and `LAMZIX`;
- result columns added by new PKNCA versions, especially `PPANMETH`, new
  `PPTESTCD` values, and CDISC mappings;
- export paths through `pivot_wider_pknca_results()` and `export_cdisc()`.

## Compatibility Checks

Before changing code:

1. Identify which PKNCA behavior is involved and whether it differs between the
   supported CRAN release and upstream development.
2. Search aNCA for existing helpers or tests that already handle the behavior.
3. Check whether PKNCA now provides a richer native definition. If it does, do
   not overwrite it with aNCA fallback registration.
4. Review linked aNCA and PKNCA issues/PRs for intended behavior.

During implementation:

- Prefer version-agnostic logic over hard-coded version checks when possible.
- Treat absent columns, `NULL` column mappings, and all-`NA` flags differently
  from explicit `FALSE` flags when PKNCA uses presence/non-`NA` as a signal.
- Resolve half-life include/exclude conflicts before calling `PKNCA::pk.nca()`
  when both flags can be active for the same profile.
- Preserve user-facing slope selector state even if computation needs a cleaned
  copy of the data.
- Keep CDISC metadata mappings in sync with new PKNCA result parameters.

## Tests And Verification

Add focused regression tests for version-sensitive behavior when logic changes.
Useful targets include:

- `tests/testthat/test-PKNCA.R`;
- `tests/testthat/test-PKNCA_extra_parameters.R`;
- `tests/testthat/test-get_halflife_plots.R`;
- `tests/testthat/test-utils-slope_selector.R`;
- `tests/testthat/test-pivot_wider_pknca_results.R`;
- `tests/testthat/test-export_cdisc.R`;
- `tests/testthat/test-intervals.R`;
- `tests/testthat/test-zip-utils.R`.

If local R is unavailable, do not install it. Instead:

- inspect source and tests directly;
- use GitHub Actions logs from the PR;
- provide manual app test steps for slope selector and export behavior.

Manual app checks for slope-related changes should cover:

- NCA with no slope modifications;
- inclusion-only manual slope selection;
- exclusion-only slope rules;
- mixed include plus exclude on the same profile;
- removing selection/exclusion rows and rerunning NCA;
- CDISC export fields such as `LAMZMTD`, `LAMZIX`, `R2ADJ`, and `PPANMETH`.

## Reporting

Report:

- which PKNCA version or upstream branch was considered;
- what aNCA behavior differed or was at risk;
- whether the fix preserves current supported PKNCA behavior;
- tests or CI jobs used as evidence;
- manual app checks still needed.
