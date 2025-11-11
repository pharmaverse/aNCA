# Transform Units

This function transforms a value from an initial unit to a target unit.

## Usage

``` r
get_conversion_factor(initial_unit, target_unit)
```

## Arguments

- initial_unit:

  A character string representing the initial unit.

- target_unit:

  A character string representing the target unit.

## Value

A numeric value for the conversion factor from the initial to the target
unit, or NA if the units are not convertible.

## Examples

``` r
get_conversion_factor("meter", "kilometer")
#> [1] 0.001
get_conversion_factor("sec", "min")
#> [1] 0.01666667
```
