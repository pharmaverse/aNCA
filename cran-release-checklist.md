# aNCA 0.2.0 release candidate

This branch prepares a proposed release for maintainer review; it does not submit
or tag a release. CRAN currently distributes 0.1.0. The existing development NEWS
entries are grouped under 0.2.0; no internal tooling changes were added to NEWS.

## Conservative package scope

- Vignettes, pkgdown sources, and development data scripts remain excluded.
- The reported Sass cache, `new-dev.zip`, generated test `Rplots.pdf`, and this
  release checklist are excluded from source builds.
- NCA calculations, public functions, settings, and exported data are unchanged.
- Optional test dependencies are guarded individually. Full-dependency checks
  still execute those tests; an absent package does not disable the whole suite.

## Reproduce validation

Use R >= 4.4, the current CRAN PKNCA (validated here with 0.12.1), and roxygen2
7.3.3. From the repository root:

```r
devtools::install_deps(dependencies = TRUE)
remotes::install_version("roxygen2", version = "7.3.3", upgrade = "never")
# Restart R after changing the installed roxygen2 version.
devtools::document()
lintr::lint_package()
devtools::test(stop_on_failure = TRUE)
devtools::check(remote = TRUE, incoming = TRUE, force_suggests = TRUE)
```

Inspect the source archive before submission, especially after local development:
there should be no `.sass_cache_keys`, `new-dev.zip`, or vignettes. The initial
candidate archive was approximately 3 MB. Keep release check output outside the
checkout so it cannot enter subsequent source builds.

## Evidence and remaining review

Local validation uses Debian 13, R 4.5.3, and CRAN PKNCA 0.12.1. Roxygen 7.3.3
completes without warnings; lint reports no findings. The full development suite
passed 4,227 expectations with no failures or
warnings (two pre-existing integration skips). The source-package check passed
4,200 expectations with no failures or warnings (three skips) and reported
0 errors, 0 warnings, and one environment time-verification NOTE. The final source
archive was 3,004,115 bytes; its contents match the checked archive except for
the automatic packaging timestamp. Details are in `cran-comments.md`.

A targeted rerun with `mockery`, `vdiffr`, and `ggh4x` genuinely absent passed
647 expectations with no failures or warnings; 47 affected/pre-existing tests
were explicitly skipped. The packages were restored after the test.

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
