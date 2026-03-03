# Create a Plotly Half-life Plot

Generates a plotly plot for NCA half-life visualization, with a fit line
and scatter points.

## Usage

``` r
get_halflife_plots(pknca_data, add_annotations = TRUE, title_vars = NULL)
```

## Arguments

- pknca_data:

  PKNCA data object

- add_annotations:

  Logical, whether to add the subtitle annotation

- title_vars:

  Character vector of additional column names to always include in the
  plot title and use as merge keys when joining concentration data with
  NCA results, even when they have only one unique level. Columns not
  present in the data are silently ignored.

## Value

A list with plotly objects and data
