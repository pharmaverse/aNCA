# Create Dose Intervals Dataset

This function creates a dataset with dose intervals and specified
pharmacokinetic parameters.

## Usage

``` r
format_pkncadata_intervals(
  pknca_conc,
  pknca_dose,
  params = c("aucinf.obs", "aucint.last", "auclast", "cmax", "half.life", "tmax",
    "lambda.z", "lambda.z.n.points", "r.squared", "adj.r.squared", "lambda.z.time.first",
    "aucpext.obs", "aucpext.pred", "clast.obs", "cl.obs"),
  start_from_last_dose = TRUE
)
```

## Arguments

- pknca_conc:

  A PKNCAdose object containing the concentration data.

- pknca_dose:

  A PKNCAdose object containing the dose data.

- params:

  A character vector specifying the pharmacokinetic parameters to
  include.

- start_from_last_dose:

  Logical defining if start is at time of last dose or C1.

## Value

A data frame containing the dose intervals and specified pharmacokinetic
parameters.

## Details

The function performs the following steps:

- Creates a vector with all pharmacokinetic parameters.

- Based on dose times, creates a data frame with start and end times.

- If TRTRINT column is present in data, sets last dose end time to
  start + TRTRINT, or if TRTRINT is NA then either Inf if only one dose
  present, or max end time if not.

- If no TRTRINT column in data, sets last dose end time to the time of
  last sample or Inf if single dose data.

- Adds logical columns for each specified parameter.

Assumes that multiple dose data will have a TRTRINT column or contain
multiple doses in dataset

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example usage:
  dose_intervals <- format_pkncadata_intervals(pknca_conc, pknca_dose, params)
} # }
```
