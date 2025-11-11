# Add impute method to the impute column

This is an internal helper function used to add an impute method to the
impute column.

## Usage

``` r
add_impute_method(impute_vals, target_impute, after)
```

## Arguments

- impute_vals:

  Character vector of impute methods.

- target_impute:

  The imputation method to be added.

- after:

  Numeric value specifying the index position in which to add the
  impute.

## Value

A character string or vector with the added impute method.
