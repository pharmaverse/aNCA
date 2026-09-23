# Keep only units that differ from their data-derived default

Change detection is value-based: a row is considered "changed" when its
target unit (`PPSTRESU`) differs from the original unit (`PPORRESU`).
This replaces the previous `default` flag, which only tracked edits made
through the Units modal and missed automatic changes such as volume
simplification.

## Usage

``` r
changed_units(units)
```

## Arguments

- units:

  A units table with `PPORRESU` and `PPSTRESU` columns.

## Value

The subset of rows where `PPSTRESU` differs from `PPORRESU`. Rows where
either unit is `NA` are excluded to avoid emitting `NA`-based entries
downstream.
