# Calculate bioavailability for intravascular vs extravascular aucs

This function calculates bioavailability (F) based on AUC (Area Under
Curve) data extracted from `res_nca`. It computes individual
bioavailability where IV and EX data are available for a subject. If IV
data is missing, it estimates bioavailability using the mean IV values
for that grouping.

## Usage

``` r
pknca_calculate_f(res_nca, f_aucs)
```

## Arguments

- res_nca:

  A list containing non-compartmental analysis (NCA) results, including
  concentration and dose data.

- f_aucs:

  A character vector of the comparing AUC parameter/s including the
  prefix f\_ (e.g., `c("f_aucinf.obs", "f_auclast")`).

## Value

A data frame with calculated absolute bioavailability values (`FABS_`)
for individual subjects where IV data is available. If IV data is
missing, it estimates bioavailability using the mean IV AUC for that
grouping.

## Details

- The function extracts AUC data from `res_nca$data$conc$data` and
  filters for selected AUC types.

- It separates data into intravascular (IV) and extravascular (EX)
  groups.

- Individual bioavailability is calculated for subjects with both IV and
  EX data using PKNCA function `pk.calc.f`.

- If IV data is missing for a subject, the function estimates
  bioavailability using mean IV values for that grouping.

- The final output includes bioavailability estimates for individual
  subjects and mean-based estimates.
