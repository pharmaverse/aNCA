# Create a Mean PK Line Plot

Generates a line plot for mean pharmacokinetic (PK) concentration-time
profiles

## Usage

``` r
exploration_meanplot(
  pknca_data,
  color_by,
  facet_by = NULL,
  show_facet_n = FALSE,
  ylog_scale = FALSE,
  threshold_value = NULL,
  show_dose = FALSE,
  palette = "default",
  sd_min = FALSE,
  sd_max = FALSE,
  ci = FALSE,
  tooltip_vars = NULL,
  labels_df = NULL,
  filtering_list = NULL,
  use_time_since_last_dose = FALSE,
  x_limits = NULL,
  y_limits = NULL
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

- show_facet_n:

  Logical; if `TRUE`, shows the number of subjects in each facet.
  Default is `FALSE`.

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

- sd_min:

  Logical; if `TRUE`, plot lower SD error bars. Default is `FALSE`.

- sd_max:

  Logical; if `TRUE`, plot upper SD error bars. Default is `FALSE`.

- ci:

  Logical; if `TRUE`, plot 95% confidence interval ribbon. Default is
  `FALSE`.

- tooltip_vars:

  Character vector of column names to include in the tooltip. Default
  includes dose group vars and "Mean".

- labels_df:

  Optional data.frame for variable label lookups (for tooltips). Default
  is `NULL`.

- filtering_list:

  Named list of filters (column = allowed values). Default is `NULL` (no
  filtering).

- use_time_since_last_dose:

  Logical; if `TRUE`, x-axis represents time since last dose. Default is
  `FALSE` (time since first dose).

- x_limits:

  Numeric vector of length 2 for x-axis limits (min, max). Default is
  `NULL` (no limits).

- y_limits:

  Numeric vector of length 2 for y-axis limits (min, max). Default is
  `NULL` (no limits).

## Value

A `ggplot` object representing the mean PK line plot, with error bars
and/or confidence intervals if requested.
