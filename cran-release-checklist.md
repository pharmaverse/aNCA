# aNCA 0.2.0 release candidate

This branch prepares a proposed release for maintainer review; it does not submit
or tag a release. CRAN currently distributes 0.1.0. The existing development NEWS
entries are grouped under 0.2.0; no internal tooling changes were added to NEWS.

## Conservative package scope

- Vignettes, pkgdown sources, development data scripts, and historical validation
  reports remain outside the source package. NEWS is included; the original
  package citation is retained.
- The reported Sass cache, `new-dev.zip`, legacy app log directory, generated
  test `Rplots.pdf`, and this
  release checklist are excluded from source builds.
- NCA computations and public function signatures are preserved. App logs now use
  a temporary or explicitly configured writable directory; only aNCA log files
  are rotated. The upload-size option is restored when `run_app()` exits.
- PKNCA extensions are initialized for namespace-only calls, including when
  only base R is attached, without replacing native upstream definitions.
  Non-dotted internal aliases preserve the original calculation functions and
  signatures while avoiding a PKNCA 0.12.1 function-lookup limitation.
- Generated scripts request explicit installation when aNCA is unavailable,
  rather than installing it automatically.
- Browser smoke coverage is opt-in with `ANCA_RUN_BROWSER_TESTS=true`; it verifies
  startup, connection, the upload UI, and absence of Shiny error outputs. SDTM
  dataset tests run against lazy-loaded installed data.
- Optional test dependencies are guarded individually. Full-dependency checks
  still execute those tests; an absent package does not disable the whole suite.

## Reproduce validation

Use R >= 4.4, CRAN PKNCA (validated here with 0.12.1), testthat >= 3.2.0,
and roxygen2 7.3.3. From the repository root:

```r
devtools::install_deps(dependencies = TRUE)
remotes::install_version("roxygen2", version = "7.3.3", upgrade = "never")
# Restart R after changing the installed roxygen2 version.
devtools::document()
lintr::lint_package()
# Optional installed-app browser smoke: ANCA_RUN_BROWSER_TESTS=true (requires Chrome).
devtools::test(stop_on_failure = TRUE)
devtools::check(remote = TRUE, incoming = TRUE, force_suggests = TRUE)
```

Inspect the source archive before submission, especially after local development:
there should be no `.sass_cache_keys`, `new-dev.zip`, or vignettes. The current
candidate archive is approximately 2.6 MB. Keep release check output outside the
checkout so it cannot enter subsequent source builds.

## Evidence and remaining review

Local validation uses Debian 13, R 4.5.3, CRAN PKNCA 0.12.1, and roxygen2
7.3.3. Focused logger, option cleanup, script generation, and SDTM tests passed.
The installed app's browser smoke and namespace-only registration tests passed
with the installed package directory made read-only for the test. Its write
permissions were restored afterward. The final package check passed 4,242
expectations with no test failures or warnings and two explicit skips. It reports 0 errors, 0 warnings, and the single
environment time-verification NOTE. The checked archive is 2,621,677 bytes.
Details are recorded in `cran-comments.md`.

The earlier preparation was also tested with `mockery`, `vdiffr`, and `ggh4x`
genuinely absent: 647 expectations passed, with no failures or warnings and 47
explicit skips for affected/pre-existing tests. Those packages were restored.

Before submitting:

- Run release checks with all suggested packages and incoming checks enabled
  on Linux, macOS, Windows, and R-devel; review the results before submission.
- Repeat remote incoming/URL checks on a network that permits CRAN. This cloud
  environment blocks CRAN and Bioconductor endpoints and cannot verify current
  time through R's remote time service; do not suppress these checks for release.
- Review the proposed version, existing NEWS, maintainer details, and the final
  `cran-comments.md`; update evidence after the checks above.
- Perform the normal scientific acceptance tests for the accumulated changes
  since 0.1.0. This preparation branch does not replace that release validation.

## Upstream PKNCA follow-up

Review on 2026-11-13 compared CRAN 0.12.1 with humanpred/pknca commit
`be353efaed885402f651238a13de34db03e41c9c` (0.12.1.9003).

The all-excluded fixture now accepts only the two specific empty concentration /
time warnings introduced upstream, while preserving its exclusion assertions.
Other warnings are not suppressed by the handler.

The VOLUME incompatibility is confirmed in the actual aNCA calculation path,
not only in a test's direct upstream call. Both the selected fork's main and
pharmaverse/aNCA main still point to `1a1c99bd459d48cbe6b6dad6556c8b3a8a82e4be`;
the upstream PKNCA main commit above is also unchanged at investigation time.

`format_pkncadata_intervals()` carries VOLUME when the concentration data contain
that column, while the default `PKNCA_update_data_object()` retained-column
option omits it. Development PKNCA now calls `assert_intervals()` in `pk.nca()`
and rejects undeclared columns. `PKNCA_calculate_nca()` does not sanitize the
intervals before its upstream call. The Shiny setup passes only extra grouping
variables for retention, so VOLUME is not automatically covered there either.
Generated scripts use the same update/calculation functions.

Reproduction used subject S1-01 / DrugA from the bundled example, with serum
parameters cmax, auclast, aucinf.obs, half.life and urine parameters ae, volpk, fe:

| Input / option | CRAN PKNCA 0.12.1 | Development PKNCA 0.12.1.9003 |
| --- | --- | --- |
| Serum, VOLUME present but entirely NA, default options | Pass | VOLUME error |
| Serum, VOLUME/VOLUMEU columns absent | Pass | Pass |
| Serum, existing VOLUME explicitly retained | Pass | Pass |
| Urine, 32 nonmissing VOLUME samples, default options | Pass | VOLUME error |
| Same urine input, existing VOLUME explicitly retained | Pass | Pass |

Both the direct `PKNCA::pk.nca()` call and `PKNCA_calculate_nca()` reproduce the
error. This confirms a user-facing compatibility problem when adopting the
upcoming dependency, while current CRAN PKNCA remains unaffected.

Two diagnostic adjustments were checked without changing repository calculation
code: declaring existing VOLUME as retained interval metadata, and removing
VOLUME from intervals while preserving it on concentration records. For both
serum and urine under both dependency versions, these alternatives produced
identical shared result columns; retaining the metadata additionally produced a
VOLUME result column. Do not blindly add VOLUME to retained columns when it is
absent: that causes a missing-column error in both versions.

A follow-up fix should align retained metadata with actual interval columns,
preserve concentration volumes used for excretion calculations, and test both
output shape and numerical results across dependency versions. It should cover
the additional excretion module too, which constructs its own interval table
and retained-column list. No runtime VOLUME fix is included in this investigation.
Track it under the existing
[PKNCA compatibility issue #481](https://github.com/pharmaverse/aNCA/issues/481).
