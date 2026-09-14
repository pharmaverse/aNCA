# Room axis labels take up along their own axis

Measures each rendered label with the plot's `axis.text` styling and
returns the extent it occupies in the direction breaks are spaced along:
width for an x axis, height for a y axis. Rotated labels contribute a
mix of the two.

## Usage

``` r
.label_extents_cm(labels, plot, axis)
```

## Arguments

- labels:

  A character vector of rendered axis labels.

- plot:

  The ggplot the labels belong to, used for the axis text theme element.

- axis:

  Axis the labels sit on, either "x" or "y".

## Value

A numeric vector of extents in centimeters, one per label.

## Details

The extent includes a space either side of the label. Without it two
labels are allowed to sit edge to edge, which reads as one run-on label
rather than two.
