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

The final packaged test run had 4,262 passed expectations, no failures or
warnings, and two explicit skips (the opt-in browser smoke and a full-session
export integration test). The development suite had 4,261 passed expectations,
no failures or warnings, and three skips (including the installed-only namespace
test). After the final registration correction, 216 focused PK/NCA expectations
also passed, and the installed base-only namespace regression passed separately.

Browser startup and namespace registration were verified with the installed app
directory made read-only. The excretion module's 20 regression assertions and
browser checks passed with both CRAN PKNCA and development 0.12.1.9003
(`be353efaed885402f651238a13de34db03e41c9c`): results render with body-weight
adjustment unchecked, empty matrix selection gives a warning, and reselection
restores results. Lint and roxygen 7.3.3 generation completed without
findings or warnings. The checked source archive is 2,623,147 bytes and includes
NEWS while excluding vignettes, historical reports, and app logs.

## Before submission

Run release checks on Linux, Windows, macOS, and R-devel;
repeat remote incoming checks on a CRAN-capable network, and
replace these candidate results with the final submission evidence. Scientific
acceptance testing for the accumulated changes since 0.1.0 remains a maintainer
release responsibility. See `cran-release-checklist.md` for the scope and the
interval-metadata compatibility checks with unreleased PKNCA.
