# Log conversions applied to dataset

This helper function logs the conversion of volume units in a dataset.

## Usage

``` r
log_conversion(row, vol, volu, u_vol_new, denom_unit, concu, verbose = TRUE)
```

## Arguments

- row:

  The row number where the conversion is applied.

- vol:

  The original volume value.

- volu:

  The original volume unit.

- u_vol_new:

  The new volume value after conversion.

- denom_unit:

  The denominator unit derived from the concentration unit.

- concu:

  The concentration unit.

- verbose:

  A logical indicating whether to log the conversion (default: TRUE).

## Value

NULL if units remained the same, or log info of the conversions that
were applied
