# Flexible Violin/Box Plot

This function generates a violin or box plot based on the provided data,
parameter, and dose information.

## Usage

``` r
flexible_violinboxplot(
  res_nca,
  parameter,
  xvars,
  colorvars,
  varvalstofilter = NULL,
  tooltip_vars,
  labels_df = metadata_nca_variables,
  box = TRUE,
  plotly = TRUE,
  seed = NULL
)
```

## Arguments

- res_nca:

  A PKNCA results object containing the results and concentration data.

- parameter:

  A string specifying the parameter to be plotted.

- xvars:

  Variables for the x axis.

- colorvars:

  Variables for the color aesthetic.

- varvalstofilter:

  Character vector specifying which variable and value to pre-filter as
  `colname: value`. By default is NULL (no pre-filtering)

- tooltip_vars:

  A character vector indicating the column names from result_data that
  should be used to identify when hovering the plotly outputs.

- labels_df:

  A data.frame used for label lookups in tooltips. Defaults to
  metadata_nca_variables.

- box:

  A logical value indicating whether to plot a box plot (`TRUE`) or a
  violin plot (`FALSE`). Default is `TRUE`.

- plotly:

  A logical value defining if the output is plotly (TRUE, default) or
  ggplot otherwise (FALSE)

- seed:

  An integer value to set the seed for reproducibility of jittering.
  Default (NULL) will use the current R seed.

## Value

A plotly object representing the violin or box plot.
