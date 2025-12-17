# Create Dose Intervals Dataset

This function creates a dataset with dose intervals and specified
pharmacokinetic parameters.

## Usage

``` r
format_pkncadata_intervals(pknca_conc, pknca_dose, start_from_last_dose = TRUE)
```

## Arguments

- pknca_conc:

  A PKNCAdose object containing the concentration data.

- pknca_dose:

  A PKNCAdose object containing the dose data.

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
adnca <- read.csv(system.file("shiny/data/example-ADNCA.csv", package = "aNCA"))
pknca_data <- PKNCA_create_data_object(adnca)
pknca_conc <- pknca_data$conc
pknca_dose <- pknca_data$dose
dose_intervals <- format_pkncadata_intervals(pknca_conc, pknca_dose)
```
