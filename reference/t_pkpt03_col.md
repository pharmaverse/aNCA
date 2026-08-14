# Summary PK Parameters Table – statistics in columns (pkpt03)

Summarizes pharmacokinetic parameters from ADPP data. Returns one data
frame per analyte (PPCAT) combination with PK parameters as rows and
descriptive statistics as columns.

## Usage

``` r
t_pkpt03_col(
  data,
  list_vars = c("PPCAT"),
  strat_var = c("TRT01A", "PARAM"),
  value_var = "AVAL",
  param_filter = NULL,
  col_group_var = NULL,
  stats = NULL
)

t_pkpt03_MP_col(data, ...)
```

## Arguments

- data:

  A CDISC ADPP data frame (from `export_cdisc()$adpp`).

- list_vars:

  Character vector of columns used to split output into separate tables.
  Default: `c("PPCAT")`. `AVISIT` is a conditional ADPP column that is
  typically absent from `export_cdisc()$adpp`; it is silently skipped
  when not present so there is no need to remove it manually, but adding
  it only helps when your ADPP actually contains visit information.

- strat_var:

  One or more columns whose combination defines the table rows
  (stratification). Default: `c("TRT01A", "PARAM")` – statistics are
  separated by treatment arm and parameter. Add e.g. `"PCSPEC"` to also
  split by specimen. Any variable that is also a `list_vars`
  (table-split) column is dropped from the rows, since it is constant
  within each split.

- value_var:

  Column containing the numeric analysis value. Default: `"AVAL"`.

- param_filter:

  Optional character vector of `PARAM` values to keep. `NULL` (default)
  keeps every parameter.

- col_group_var:

  Optional subject-level column (e.g. `"SEX"`, `"RACE"`) whose values
  become side-by-side comparison column groups: the full statistic block
  is repeated once per level, nested under a group header. `NULL`
  (default) produces the standard flat table. Must differ from
  `strat_var` and the `list_vars`.

- stats:

  Optional character vector of statistics to display, chosen from
  `c("n", "Mean", "SD", "CV_pct", "GeoMean", "GeoCV_pct", "Median", "Min", "Max")`.
  `NULL` (default) shows all of them. Names not produced by this table
  are ignored.

- ...:

  Additional arguments forwarded to `t_pkpt03_col()`.

## Value

A named list of data frames, one per combination of `list_vars`. Each
data frame has one column per `strat_var` followed by the statistics:
`n`, `Mean`, `SD`, `CV_pct`, `GeoMean`, `GeoCV_pct`, `Median`, `Min`,
`Max`. When `col_group_var` is set, the statistic columns are prefixed
per group level and a `col_groups` attribute drives the rendered
two-level header.

## Functions

- `t_pkpt03_MP_col()`: Summary of metabolite-to-parent ratios (stats in
  columns). Filters to metabolite rows using `METABFL` (preferred) or,
  when absent from ADPP, falls back to rows where `PPCAT` or `PARAM`
  contains "metab" (case-insensitive). `METABFL` is present in ADPP only
  when it was included as a grouping variable in the NCA run.

## Examples

``` r
if (FALSE) { # \dontrun{
adpp <- export_cdisc(res_nca)$adpp
tables <- t_pkpt03_col(adpp)
tables[[1]]
# Separate statistics by specimen too:
t_pkpt03_col(adpp, strat_var = c("TRT01A", "PARAM", "PCSPEC"))[[1]]
# Compare sexes side by side:
t_pkpt03_col(adpp, col_group_var = "SEX")[[1]]
} # }
```
