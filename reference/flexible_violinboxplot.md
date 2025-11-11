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
  columns_to_hover,
  box = TRUE,
  plotly = TRUE
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

- columns_to_hover:

  A character vector indicating the column names from result_data that
  should be used to identify when hovering the plotly outputs

- box:

  A logical value indicating whether to plot a box plot (`TRUE`) or a
  violin plot (`FALSE`). Default is `TRUE`.

- plotly:

  A logical value defining if the output is plotly (TRUE, default) or
  ggplot otherwise (FALSE)

## Value

A plotly object representing the violin or box plot.
