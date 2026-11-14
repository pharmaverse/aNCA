# Manual check: Recover from invalid excretion inputs and calculation errors

Base: `chore/cran-review-fixes`. Fix: `fix/cran-excretion`.

Compare `chore/cran-review-fixes` with this fix branch in **fresh R sessions**. Keep downloaded CSVs/probes outside the checkout so switching branches does not remove them. Use CRAN PKNCA 0.12.1 unless this guide explicitly requests development PKNCA.

For app checks, run from the selected checkout:

```r
devtools::load_all(export_all = FALSE)
aNCA::run_app()
```

Use Data → Upload, then Next → Mapping → Next → Filtering → Next → Preview. Keep same-name column mappings and apply no filters. For the supplied CSVs, check the mappings named below before continuing.

**Data:** the default data reproduce the end-time error. The paired CSVs reproduce invalid weight units: `excretion-invalid-weight-unit.csv` and `excretion-valid.csv`. The default data do not contain the required weight fields.

**End-time case:** with the default data mapped, open NCA → Additional Analysis → Excretion. Select URINE, uncheck Adjust for Body Weight, select RCAMINT and Samples, map the end time to AFRLT, then Submit.

- **CRAN branch:** a calculation error can disconnect the session.
- **Fix branch:** a collection-time warning appears and the session stays connected. Change the end column to AEFRLT and resubmit; results appear.

**Weight-unit case:** upload the invalid CSV. Confirm mappings AEFRLT, VOLUME, VOLUMEU, WTBL and WTBLU. In Excretion select URINE, AEFRLT, RCAMINT and FREXINT, Profiles only, and check Adjust for Body Weight. Submit.

- **CRAN branch:** the weight-adjustment calculation errors; the session can disconnect.
- **Fix branch:** a recognized-mass-unit warning appears without disconnection. Upload the valid CSV, repeat the same selections, and submit: RCAMINT = 100 µg and FREXINT = 1%.

The two CSVs differ only in WTBLU (`unknown` versus `kg`). Four samples contain concentrations 1, 2, 3, 4 µg/mL and volumes 10 mL each. Dose = 5 mg/kg, weight = 2 kg. Their total is 100 µg / 10 mg = 1%.

Files: [excretion-valid.csv](data/excretion-valid.csv) · [excretion-invalid-weight-unit.csv](data/excretion-invalid-weight-unit.csv).
