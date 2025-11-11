# Prepare Data for PK Lineplot

Prepare Data for PK Lineplot

## Usage

``` r
preprocess_data_for_plot(
  data,
  selected_usubjids,
  selected_analytes,
  selected_pcspec,
  colorby_var,
  time_scale,
  yaxis_scale,
  cycle
)
```

## Arguments

- data:

  Raw data frame.

- selected_usubjids, selected_analytes, selected_pcspec, cycle:

  Inputs for filtering.

- colorby_var:

  The variable(s) to be used for coloring.

- time_scale:

  String, either "By Dose Profile" or "Actual Time".

- yaxis_scale:

  String, either "Log" or "Linear".

## Value

A processed and filtered data.frame.
