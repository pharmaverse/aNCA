# Manual check: Complete repeated settings restores

Base: `chore/cran-review-fixes`. Fix: `fix/cran-restore`.

Compare `chore/cran-review-fixes` with this fix branch in **fresh R sessions**. Keep downloaded CSVs/probes outside the checkout so switching branches does not remove them. Use CRAN PKNCA 0.12.1 unless this guide explicitly requests development PKNCA.

For app checks, run from the selected checkout:

```r
devtools::load_all(export_all = FALSE)
aNCA::run_app()
```

Use Data → Upload, then Next → Mapping → Next → Filtering → Next → Preview. Keep same-name column mappings and apply no filters. For the supplied CSVs, check the mappings named below before continuing.

**Data:** default `adnca_example`; no CSV needed.

1. Map the default data, open NCA → Setup → Download settings → Save. Do not run NCA for this minimal test.
2. Save two copies of the downloaded YAML under different filenames, e.g. `restore-a.yaml` and `restore-b.yaml`.
3. In the same app session, upload A through Data → Upload and choose Restore if the version dialog appears. Wait for completion, then upload B and restore it too.

**CRAN branch:** the first restore completes; the second can leave “Restoring session…” on screen because completion is not signalled again.

**Fix branch:** both complete and dismiss the modal. Repeat a third time. Optionally repeat with a saved session created after Run NCA to check repeated NCA auto-run too.
