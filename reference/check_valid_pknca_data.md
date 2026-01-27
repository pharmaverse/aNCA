# Checks Before Running NCA

This function checks that:

1.  check_exclusion_has_reason: all manually excluded half-life points
    in the concentration data have a non-empty reason provided. If any
    exclusions are missing a reason, it stops with an error and prints
    the affected rows (group columns and time column).

## Usage

``` r
check_valid_pknca_data(processed_pknca_data, check_exclusion_has_reason = TRUE)
```

## Arguments

- processed_pknca_data:

  A processed PKNCA data object.

- check_exclusion_has_reason:

  Logical; Check if all exclusions have a reason (default: TRUE).

## Value

The processed_pknca_data object (input), if checks are successful.

## Details

- If any excluded half-life points are missing a reason, an error is
  thrown.

- If no exclusions or all have reasons, the function returns the input
  object.

- Used to enforce good practice/documentation before NCA calculation.

## Examples

``` r
# Suppose processed_pknca_data is a valid PKNCA data object
# check_valid_pknca_data(processed_pknca_data)
```
