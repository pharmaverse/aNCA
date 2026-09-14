# Format numeric axis breaks for display

Renders whole numbers without a decimal part, so a break at `12` is
labeled `12` rather than `12.0`, and leaves fractional breaks such as
`-0.083` untouched. Used both as the `labels` argument of the
concentration plot scales and as the default of
[`filter_breaks()`](https://pharmaverse.github.io/aNCA/reference/filter_breaks.md),
so the breaks are filtered against the labels that actually get drawn.

## Usage

``` r
format_axis_labels(x)
```

## Arguments

- x:

  A numeric vector of break positions.

## Value

A character vector of labels, one per element of `x`.
