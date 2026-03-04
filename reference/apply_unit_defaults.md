# Apply default target units onto a data-derived units table

Takes a table of desired target units (with `PPTESTCD` and `PPSTRESU`)
and merges it with a data-derived units table to fill in `PPORRESU` and
calculate `conversion_factor`. Parameters not present in the defaults
retain their data-derived values.

## Usage

``` r
apply_unit_defaults(default_units, data_units)
```

## Arguments

- default_units:

  A data frame with at least `PPTESTCD` and `PPSTRESU` columns.

- data_units:

  A data frame with the full data-derived units table (`PPTESTCD`,
  `PPORRESU`, `PPSTRESU`, `conversion_factor`, and optional group
  columns).

## Value

A list with two elements:

- `units` - The complete units table with defaults applied.

- `failed` - A data frame of rows where conversion factor could not be
  calculated.
