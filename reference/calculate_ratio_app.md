# Links the table ratio of the App with the ratio calculations via PKNCA results

Links the table ratio of the App with the ratio calculations via PKNCA
results

## Usage

``` r
calculate_ratio_app(
  res,
  test_parameter,
  ref_parameter = test_parameter,
  test_group = "(all other levels)",
  ref_group = "PARAM: Analyte01",
  aggregate_subject = "no",
  adjusting_factor = 1,
  custom_pptestcd = NULL
)
```

## Arguments

- res:

  A PKNCAresult object.

- test_parameter:

  Character. The PPTESTCD value to use as test (numerator). May include
  a range suffix for interval parameters (e.g. `AUCINT_0-20`).

- ref_parameter:

  Character. The PPTESTCD value to use as reference (denominator).
  Defaults to test_parameter. May include a range suffix.

- test_group:

  Character. The test group (numerator). Default is "(all other
  levels)".

- ref_group:

  Character. The reference group (denominator).

- aggregate_subject:

  Character. Aggregation mode: "yes", "no", or "if-needed".

- adjusting_factor:

  Numeric that multiplies the calculated ratio. Default is 1.

- custom_pptestcd:

  Optional character. If provided, will be used as the PPTESTCD value.

## Value

A data.frame with the calculated ratios for the specified settings.
