# Helper function to handle optional layers

Helper function to handle optional layers

## Usage

``` r
add_optional_layers(
  plt,
  yaxis_scale,
  show_threshold,
  threshold_value,
  show_dose,
  data,
  time_scale,
  facet_by = NULL
)
```

## Arguments

- plt:

  The ggplot object to modify

- yaxis_scale:

  The scale of the y-axis ("Log" or "Linear")

- show_threshold:

  Whether to show a threshold line

- threshold_value:

  The value of the threshold line

- show_dose:

  Whether to show dose times as vertical lines

- data:

  The data used for plotting

- time_scale:

  The time scale used for plotting

- facet_by:

  Variables to facet the plot by \#' @returns The modified ggplot object
  with optional layers added
