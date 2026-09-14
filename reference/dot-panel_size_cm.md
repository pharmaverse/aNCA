# Room a single panel gets along one axis

Reads the laid-out size of a panel from the plot's `gtable`. The panel
border grob cannot be used for this: its width and height are
`unit(1, "npc")`, which converts to the size of the whole device,
ignoring both the space the axes and titles take and the fact that a
faceted plot splits what is left between several panels.

## Usage

``` r
.panel_size_cm(plot_table, axis)
```

## Arguments

- plot_table:

  A `gtable`, as returned by
  [`ggplot2::ggplot_gtable()`](https://ggplot2.tidyverse.org/reference/ggplot_gtable.html).

- axis:

  Axis to measure along, either "x" or "y".

## Value

The size of one panel in centimeters.
