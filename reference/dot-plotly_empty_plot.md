# Generate an Empty Plotly Object

This function returns a blank Plotly plot with optional annotation text.
It ensures that when no valid data is available, a meaningful
placeholder plot is displayed instead of causing an error.

## Usage

``` r
.plotly_empty_plot(message = "No data available")
```

## Arguments

- message:

  A character string specifying the text to display in the center of the
  empty plot. Defaults to `"No data available"`.

## Value

A Plotly object representing an empty plot with hidden axes.
