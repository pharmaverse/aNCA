# Helper to create a Quarto code chunk for a plot

Used internally to format a plot chunk for Quarto documents.

## Usage

``` r
add_qmd_plot(plot_expr, use_plotly = FALSE)
```

## Arguments

- plot_expr:

  Expression for plot.

- use_plotly:

  Logical, whether to convert plot to plotly.

## Value

Character vector for Quarto code chunk.
