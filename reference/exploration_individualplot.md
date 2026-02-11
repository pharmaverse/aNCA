# Create an Individual PK Line Plot

Generates a line plot for individual pharmacokinetic (PK)
concentration-time profiles

## Usage

``` r
exploration_individualplot(
  pknca_data,
  color_by,
  facet_by = NULL,
  ylog_scale = FALSE,
  threshold_value = NULL,
  show_dose = FALSE,
  palette = "default",
  tooltip_vars = NULL,
  labels_df = NULL,
  filtering_list = NULL,
  use_time_since_last_dose = FALSE
)
```

## Arguments

- pknca_data:

  A PKNCAdata object containing the raw PK concentration data.

- color_by:

  Character vector specifying the column(s) used to color the lines and
  points.

- facet_by:

  Character vector of column names to facet the plot by. Default is
  `NULL` (no faceting).

- ylog_scale:

  Logical; whether to use a logarithmic scale for the y-axis. Default is
  `FALSE`.

- threshold_value:

  Numeric; y-intercept for a horizontal threshold line. Default is
  `NULL` (no threshold).

- show_dose:

  Logical; if `TRUE`, vertical lines for dose times are shown. Default
  is `FALSE`.

- palette:

  Optional palette name or named character vector of colors for the
  plot. Default is "default" color palette.

- tooltip_vars:

  Character vector of column names to include in the tooltip. Default is
  `NULL`.

- labels_df:

  Optional data.frame for variable label lookups (for tooltips). Default
  is `NULL`.

- filtering_list:

  Named list of filters (column = allowed values). Default is `NULL` (no
  filtering).

- use_time_since_last_dose:

  Logical; if `TRUE`, x-axis represents time since last dose. Default is
  `FALSE` (time since first dose).

## Value

A `ggplot` object representing the individual PK line plot.
