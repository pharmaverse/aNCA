# Filter Breaks for X-Axis

Filters X-axis for consecutive breaks with at least the specified
distance.

## Usage

``` r
filter_breaks(breaks = NA, plot = plot, min_cm_distance = 0.5, axis = "x")
```

## Arguments

- breaks:

  A numeric vector of x-axis breaks.

- plot:

  A ggplot object used to extract plot dimensions and scales.

- min_cm_distance:

  A numeric of the minimum distance between breaks.

- axis:

  Axis to filter on, either "x" or "y".

## Value

A numeric vector of filtered x-axis breaks.

## Author

Gerardo Rodriguez
