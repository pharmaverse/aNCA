# Remove impute method from the impute column

This is an internal helper function used to remove an impute method from
the impute column.

## Usage

``` r
remove_impute_method(impute_vals, target_impute)
```

## Arguments

- impute_vals:

  Character vector of impute methods.

- target_impute:

  The imputation method to be removed.

## Value

A character string or vector without the specified impute method.

## Details

Resulting empty string values are replaced with NA_character\_.
