# Apply Ratio Calculations to PKNCAresult Object

This function takes a PKNCAresult object and a data.frame specifying
ratio calculations.

## Usage

``` r
calculate_table_ratios(res, ratio_table)
```

## Arguments

- res:

  A PKNCAresult object.

- ratio_table:

  Data.frame with columns: TestParameter, RefParameter, Reference, Test,
  AggregateSubject, AdjustingFactor, TestGroups, RefGroups, PPTESTCD.

## Value

The updated PKNCAresult object with added rows in the `result`
data.frame.
