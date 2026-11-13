## Release candidate

This is a proposed update from CRAN aNCA 0.1.0 to 0.2.0, prepared for maintainer
review. It has not been submitted to CRAN.

## Test environment

* Debian GNU/Linux 13, x86_64
* R 4.5.3
* PKNCA 0.12.1 (CRAN release)
* roxygen2 7.3.3

## R CMD check results

0 errors | 0 warnings | 1 NOTE

* checking for future file timestamps: unable to verify current time

The check ran with `--as-cran --no-manual`, incoming feasibility checks enabled,
and all suggested packages required and installed. Remote incoming checks were
not run because this cloud environment blocks CRAN endpoints. The remaining
NOTE is an environment time-verification limitation; it has not been suppressed.

The packaged test run had 4,200 passed expectations, no failures or warnings,
and three skipped tests (two existing integration tests and the installed-package
SDTM fixture-file guard). The development test run had 4,227 passed expectations,
no failures or warnings, and two existing integration tests skipped. Lint and
roxygen generation completed without findings or warnings.

## Before submission

Run release checks on Linux, Windows, macOS, and R-devel;
repeat remote incoming checks on a CRAN-capable network, and
replace these candidate results with the final submission evidence. Scientific
acceptance testing for the accumulated changes since 0.1.0 remains a maintainer
release responsibility. See `cran-release-checklist.md` for the scope and the
separate unreleased-PKNCA compatibility risk.
