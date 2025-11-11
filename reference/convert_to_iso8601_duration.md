# Convert Numeric Value and Unit to ISO 8601 Duration

The function converts a numeric value and its associated time unit into
ISO 8601 duration string.

## Usage

``` r
convert_to_iso8601_duration(value, unit)
```

## Arguments

- value:

  A numeric value representing the time/duration.

- unit:

  A character string representing the unit of the time/duration.

## Value

A character string representing the duration in ISO 8601 format.

## Details

It is a sensitive function that assumes that a valid time unit is given
by the user. That means that if other units starting with 'y', 'm', 'w',
'd', 'h', or 's' are provided, it will make a naive guess that it refers
to a time unit as year, month, week, day, hour, sec...

## Examples

``` r
aNCA:::convert_to_iso8601_duration(200, "h") # Returns "PT200H"
#> [1] "PT200H"
aNCA:::convert_to_iso8601_duration(5, "d")  # Returns "P5D"
#> [1] "P5D"
```
