# Conditionally Verify and Override PK Parameters Based on Sample Type

This helper function updates a PKNCA intervals data frame by verifying
and overriding specific pharmacokinetic parameters depending on whether
the sample is identified as excreta (e.g., urine, feces, bile).
Parameters related to excretion (such as `ae`, `fe`, and those starting
with `"clr."`) are selectively enabled only for excreta samples and set
to `FALSE` otherwise.

## Usage

``` r
verify_parameters(pknca_intervals, params, all_pknca_params)
```

## Arguments

- pknca_intervals:

  A data frame containing PKNCA interval information, including
  pharmacokinetic parameters and a `PCSPEC` column that describes the
  specimen type.

- params:

  A character vector of parameter names selected by the user. Only these
  parameters will remain `TRUE` for excreta types.

- all_pknca_params:

  A character vector of all pharmacokinetic parameters that may be
  present in `pknca_intervals`. These will be checked and updated
  accordingly.

## Value

A modified version of the `pknca_intervals` data frame with appropriate
parameters updated based on the specimen type.
