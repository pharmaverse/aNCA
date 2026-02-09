# Apply Slope Rules to Update Data

Iterates over the given rules and updates the PKNCA object setting
inclusion/exclusion flags.

## Usage

``` r
update_pknca_with_rules(data, slopes)
```

## Arguments

- data:

  PKNCA data object

- slopes:

  Data frame of slope rules (TYPE, RANGE, REASON, group columns)

## Value

Modified data object with updated flags
