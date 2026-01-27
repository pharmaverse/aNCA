# Filter dataset based on slope selections and exclusions

This function filters main dataset based on provided slope selections an
exclusions.

## Usage

``` r
filter_slopes(data, slopes, profiles, slope_groups)
```

## Arguments

- data:

  Data to filter. Must be `PKNCAdata` list, containing the `conc`
  element with `PKNCAconc` list and appropriate data frame included
  under data.

- slopes:

  A data frame containing slope rules, including `TYPE`, `RANGE`, and
  `REASON` columns. May also have grouping columns (expected to match
  slope_groups)

- profiles:

  List with available profiles for each `SUBJECT`.

- slope_groups:

  List with column names that define the groups.

## Value

Original dataset, with `is.included.hl`, `is.excluded.hl` and
`exclude_half.life` columns modified in accordance to the provided slope
filters.
