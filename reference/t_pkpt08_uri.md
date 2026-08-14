# Mean Urine Amount and Percent Recovered Table (pkpt08)

Filters ADPP to urine specimen records and summarizes cumulative amount
excreted (Ae) and percentage of dose recovered (Fe%) with descriptive
statistics in columns. Per the TLG catalog specification for pkpt08, the
summary includes n, Mean, SD, CV%, Median, Min, Max – without geometric
mean or geometric CV% (those are omitted because urine recovery
parameters are not log-normally distributed by convention).

## Usage

``` r
t_pkpt08_uri(
  data,
  urine_specs = c("URINE"),
  list_vars = c("PPCAT"),
  strat_var = c("TRT01A", "PARAM"),
  value_var = "AVAL",
  param_filter = NULL,
  col_group_var = NULL,
  stats = NULL
)
```

## Arguments

- data:

  A CDISC ADPP data frame. Urine records are identified by
  `PPSPEC %in% urine_specs`.

- urine_specs:

  Character vector of specimen types considered urine, matched
  case-insensitively. Default: `c("URINE")`.

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

## Value

Named list of data frames with one column per `strat_var` followed by
`n`, `Mean`, `SD`, `CV_pct`, `Median`, `Min`, `Max`. Use
[`t_pkpt03_col()`](https://pharmaverse.github.io/aNCA/reference/t_pkpt03_col.md)
instead if geometric mean statistics are needed.

## Examples

``` r
if (FALSE) { # \dontrun{
adpp <- export_cdisc(res_nca)$adpp
tables <- t_pkpt08_uri(adpp)
} # }
```
