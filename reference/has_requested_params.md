# Check whether any interval rows have at least one requested parameter

Inspects the logical parameter columns in an intervals data frame and
returns `TRUE` if any row has at least one of the specified parameters
set to `TRUE`.

## Usage

``` r
has_requested_params(intervals, params)
```

## Arguments

- intervals:

  A data frame of PKNCA intervals.

- params:

  Character vector of parameter column names to check.

## Value

A single logical value.
