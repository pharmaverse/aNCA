# Helper: adjust class and length (optional) for a data.frame based on metadata_nca_variables

Helper: adjust class and length (optional) for a data.frame based on
metadata_nca_variables

## Usage

``` r
adjust_class_and_length(df, metadata, adjust_length = TRUE)
```

## Arguments

- df:

  Data frame to adjust.

- metadata:

  Metadata data frame with variable specifications.

- adjust_length:

  Logical indicating whether to adjust length of variables.

## Value

Adjusted data frame.

## Details

Adjusts the class and length of each variable in the provided data frame
according to the specifications in the metadata data frame. Only adjusts
the length if `adjust_length = TRUE`
