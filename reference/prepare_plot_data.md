# Prepare Data for PK Dose QC Plotting

A helper function that validates, processes, and combines concentration
and dose data. It creates the unified legend and faceting variables and
calculates the factor levels for the plot scales.

## Usage

``` r
prepare_plot_data(
  data_conc,
  data_dose,
  shape_var,
  colour_var,
  grouping_vars,
  labels_df,
  tooltip_vars,
  plot_conc_data,
  plot_dose_data
)
```

## Arguments

- data_conc:

  A data.frame of concentration data.

- data_dose:

  An optional data.frame of dosing data.

- shape_var:

  Character. The column name from `data_conc` for the legend.

- colour_var:

  Character. The column name from `data_dose` for the legend.

- grouping_vars:

  Character vector. Column names for faceting.

- labels_df:

  A data.frame for label lookups.

- tooltip_vars:

  Character vector of variables for the tooltip.

- plot_conc_data:

  Logical flag derived from `show_pk_samples` and `data_conc`.

- plot_dose_data:

  Logical flag derived from `show_doses` and `data_dose`.

## Value

A `list` containing `data` (the processed tibble), `shape_levels`, and
`colour_levels`.
