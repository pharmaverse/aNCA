# Identify target rows based on groups, parameters, and impute method

This is an internal helper function used to identify the target rows in
the data frame based on the specified groups, parameters, and impute
method.

## Usage

``` r
identify_target_rows(
  data,
  target_impute,
  target_params,
  target_groups,
  after = NULL
)
```

## Arguments

- data:

  A data frame containing the intervals.

- target_impute:

  The imputation method to be added or removed.

- target_params:

  A character vector specifying the parameters to be targeted.

- target_groups:

  A data frame specifying the intervals to be targeted.

- after:

  Numeric value specifying the index position in which to add the impute
  (optional).

## Value

A logical vector indicating the target rows.
