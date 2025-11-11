# Calculate Ratios from PKNCA Results

Calculate Ratios from PKNCA Results

## Usage

``` r
calculate_ratios(
  data,
  test_parameter,
  ref_parameter = test_parameter,
  match_cols,
  ref_groups,
  test_groups = NULL,
  adjusting_factor = 1,
  custom_pptestcd = NULL
)
```

## Arguments

- data:

  A PKNCAresults object or its result data.frame.

- test_parameter:

  Character. The PPTESTCD value to use as test (numerator).

- ref_parameter:

  Character. The PPTESTCD value to use as reference (denominator).
  Defaults to test_parameter.

- match_cols:

  Character vector of column names to match between test and reference
  groups or a data.frame specifying columns and values.

- ref_groups:

  A data.frame specifying reference groups. At its minimum, contains the
  contrast variable value(s) for the reference.

- test_groups:

  A data.frame specifying test groups. Optional. By default is NULL,
  allowing rows not in ref_groups be used as test.

- adjusting_factor:

  Numeric to multiply the ratio. Default is 1.

- custom_pptestcd:

  Optional character. If provided, will be used as the PPTESTCD value.

## Value

A data.frame result object with the calculated ratios.
