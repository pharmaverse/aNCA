# Filter Breaks for X-Axis

Filters an axis for consecutive breaks that are far enough apart to be
drawn without their labels colliding. A break is kept when its distance
from the last kept break is at least `min_cm_distance` *and* at least
the space the two labels occupy along the axis, so a wide label such as
`119.917` thins the axis further than a short one such as `12` does.

## Usage

``` r
filter_breaks(
  breaks = NA,
  plot = plot,
  min_cm_distance = 0.5,
  axis = "x",
  labels = format_axis_labels
)
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

- labels:

  A function turning `breaks` into the labels that will be drawn, used
  to measure how much room each one needs. Defaults to the same labels
  the concentration plots draw.

## Value

A numeric vector of filtered x-axis breaks.

## Details

Candidates outside the plotted range are dropped before any of that, so
a break that is never drawn cannot decide which of the visible ones
survive.

## Author

Gerardo Rodriguez
