# Add a slide to the Quarto document with a plot and two tables (side by side)

Used internally for dose escalation reporting.

## Usage

``` r
add_qmd_sl_plottabletable(quarto_path, df1, df2, plot, use_plotly = FALSE)
```

## Arguments

- quarto_path:

  Path to the Quarto (.qmd) file to append to.

- df1:

  Expression for first table (left column).

- df2:

  Expression for second table (right column).

- plot:

  Expression for plot.

- use_plotly:

  Logical, whether to convert plot to plotly.

## Value

Invisibly returns TRUE if the slide was added.
