# Apply Slope Rules to Update Data

This function iterates over the given slopes and updates the
`data$conc$data` object by setting inclusion or exclusion flags based on
the slope conditions.

## Usage

``` r
.apply_slope_rules(data, slopes, slope_groups)
```

## Arguments

- data:

  A list containing concentration data (`data$conc$data`) with columns
  that need to be updated based on the slope rules.

- slopes:

  A data frame containing slope rules, including `TYPE`, `RANGE`, and
  `REASON` columns. May also have grouping columns (expected to match
  slope_groups)

- slope_groups:

  A character vector specifying the group columns used for filtering.

## Value

description The modified `data` object with updated inclusion/exclusion
flags and reasons in `data$conc$data`.
