# Calculate bioavailability with pivoted output

This function calculates bioavailability (F) based on AUC (Area Under
Curve) data extracted from `res_nca`. It computes individual
bioavailability where IV and EX data are available for a subject. If IV
data is missing, it estimates bioavailability using the mean IV values
for that grouping. The output is pivoted such that each row represents
all main results summarized for each profile in each subject. Columns
are assumed to be in `%` units even if not explicitly stated.

## Usage

``` r
calculate_f(res_nca, f_aucs)
```

## Arguments

- res_nca:

  A list containing non-compartmental analysis (NCA) results, including
  concentration and dose data.

- f_aucs:

  A character vector of the comparing AUC parameter/s including the
  prefix f\_ (e.g., `c("f_aucinf.obs", "f_auclast")`).

## Value

A pivoted data frame with bioavailability calculations (`f_aucinf`,
`f_auclast`, etc.) for individual subjects where IV data is available.
If IV data is missing for the subject, the mean IV AUC for that group is
used instead. Variables are assumed to be in `%` units.
