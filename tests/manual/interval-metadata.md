# Manual check: Preserve NCA compatibility and recover from slope plot errors

Base: `chore/cran-review-fixes`. Fix: `fix/cran-interval-metadata`.

Compare `chore/cran-review-fixes` with this fix branch in **fresh R sessions**. Keep downloaded CSVs/probes outside the checkout so switching branches does not remove them. Use CRAN PKNCA 0.12.1 unless this guide explicitly requests development PKNCA.

For app checks, run from the selected checkout:

```r
devtools::load_all(export_all = FALSE)
aNCA::run_app()
```

Use Data → Upload, then Next → Mapping → Next → Filtering → Next → Preview. Keep same-name column mappings and apply no filters. For the supplied CSVs, check the mappings named below before continuing.

**Data:** default `adnca_example`; no CSV needed. This VOLUME error requires development PKNCA 0.12.1.9003, tested at `be353efaed885402f651238a13de34db03e41c9c`. CRAN PKNCA 0.12.1 does not reproduce the strict VOLUME rejection. Restart R when changing dependency versions.

1. Map the default data, select DrugA and SERUM in NCA Setup, and include LAMZHL in Parameter Selection. Open Slope Selector, then Run NCA if the session remains connected.
2. Repeat on the fix branch with the same dependency version and data. Also check urine parameters on URINE.

**CRAN branch + development PKNCA:** automatic slope generation or Run NCA reports that VOLUME is not allowed; the slope observer can disconnect the session.

**Fix branch:** slope plots and NCA calculate without that error. Concentration sample volumes remain present.

**Controlled recovery / callback check:** run the supplied `test-interval-metadata.R` probe below. With development PKNCA the base branch fails on VOLUME and the forced slope failure closes its mock session. The fix passes all 24 assertions, clears failed plots, and recovers after correction. The probe injects its own temporary failure; no deliberately broken dataset is needed.

Repeat the probe with CRAN PKNCA 0.12.1 to check fallback callback lookup. On the base, urine calculations can report `anca_calc_volpk` missing. The fix resolves volume and excretion-rate callbacks. To check the real installation, install the selected branch and restart R before loading `library(aNCA)`; default `load_all()` with all internal functions exported can conceal this lookup bug.
Stop the app first. In a fresh R session in the selected checkout, run this command and choose the supplied probe file from the downloaded bundle:

```r
devtools::load_all(export_all = FALSE)
testthat::test_file(
  file.choose(),
  env = new.env(parent = asNamespace("aNCA")),
  stop_on_failure = TRUE
)
```

The same probe loads production code from whichever branch is checked out. Failures on the CRAN branch are expected.

Files: [test-interval-metadata.R](../testthat/test-interval-metadata.R).
