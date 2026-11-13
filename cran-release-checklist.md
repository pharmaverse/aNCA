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

A focused development-PKNCA run found a separate incompatibility: the start
imputation regression test calls upstream `pk.nca()` with a `VOLUME` interval
column that newer upstream validation rejects. The run had 242 passed
expectations and one error, plus six fixture setup warnings about automatic dose
intervals and mixed routes. This is an upgrade/compatibility risk, not a failure
with the supported CRAN dependency. No calculation behavior was changed to
accommodate an unreleased dependency in this branch. Track it under the existing
[PKNCA compatibility issue #481](https://github.com/pharmaverse/aNCA/issues/481)
before adopting that upstream version.
