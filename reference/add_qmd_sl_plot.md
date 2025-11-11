# Add a slide to the Quarto document with a single plot

Used internally for dose escalation reporting.

## Usage

``` r
add_qmd_sl_plot(quarto_path, plot, use_plotly = FALSE)
```

## Arguments

- quarto_path:

  Path to the Quarto (.qmd) file to append to.

- plot:

  Expression for plot.

- use_plotly:

  Logical, whether to convert plot to plotly.

## Value

Invisibly returns TRUE if the slide was added.
