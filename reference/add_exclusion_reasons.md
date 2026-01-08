# Add Exclusion Reasons to PKNCAdata Object

This function adds exclusion reasons to the `exclude` column of the
concentration object within a PKNCAdata object, based on a list of
reasons and row indices.

## Usage

``` r
add_exclusion_reasons(pknca_data, exclusion_list)
```

## Arguments

- pknca_data:

  A PKNCAdata object.

- exclusion_list:

  A list of lists, each with elements:

  - reason: character string with the exclusion reason (e.g.,
    "Vomiting")

  - rows: integer vector of row indices to apply the reason to

## Value

The modified PKNCAdata object with updated exclusion reasons in the
concentration object.
