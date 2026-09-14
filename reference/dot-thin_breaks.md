# Keep the breaks that clear both the minimum gap and their neighbors' labels

A single greedy pass from the leftmost break: each candidate is measured
against the last one kept, and kept itself only if the gap covers both
`min_cm_distance` and the room the two labels either side of it need.

## Usage

``` r
.thin_breaks(
  breaks,
  label_sizes_cm,
  min_cm_distance,
  scale_range,
  panel_size_cm
)
```

## Arguments

- breaks:

  A sorted numeric vector of candidate breaks, already trimmed to the
  plotted range.

- label_sizes_cm:

  Room each label needs along the axis, one per break.

- min_cm_distance:

  A numeric of the minimum distance between breaks.

- scale_range:

  The panel's range along the axis, used to convert break positions into
  centimeters.

- panel_size_cm:

  The size of the panel along the axis, in centimeters.

## Value

A numeric vector of the breaks that survive.
