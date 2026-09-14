# Replace a concentration plot's x scale with filtered breaks

Applies
[`filter_breaks()`](https://pharmaverse.github.io/aNCA/reference/filter_breaks.md)
to `break_values` and puts the surviving breaks on the plot's x scale.
Must be added after any faceting, because a faceted plot splits the
panel and so has room for fewer labels than the same plot drawn as a
single panel.

## Usage

``` r
add_filtered_x_scale(plot, break_values, min_cm_distance)
```

## Arguments

- plot:

  A ggplot object, complete apart from its x scale.

- break_values:

  A numeric vector of candidate breaks.

- min_cm_distance:

  A numeric of the minimum distance between breaks.

## Value

The plot with its x scale replaced.
