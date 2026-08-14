# Mean Dose-Normalized PK Parameters Table (pkpt07)

Filters ADPP to dose-normalized parameters and summarizes them with the
same column layout as
[`t_pkpt03_col()`](https://pharmaverse.github.io/aNCA/reference/t_pkpt03_col.md).
These parameters must have been computed during the NCA run – they are
not derived on the fly.

## Usage

``` r
t_pkpt07_norm(
  data,
  paramcd_var = "PARAMCD",
  paramcd_filter = c("CMAXD", "AUCLSTD", "AUCIFOD", "AUCTLSTD"),
  list_vars = c("PPCAT"),
  strat_var = c("TRT01A", "PARAM"),
  value_var = "AVAL",
  col_group_var = NULL,
  stats = NULL
)
```

## Arguments

- data:

  A CDISC ADPP data frame (from `export_cdisc()$adpp`).

- paramcd_var:

  Column containing parameter codes used to detect dose-normalized
  parameters. Default: `"PARAMCD"`.

- paramcd_filter:

  Character vector of CDISC dose-normalized PARAMCDs to keep. Defaults
  to the standard codes used in this package:
  `c("CMAXD", "AUCLSTD", "AUCIFOD", "AUCTLSTD")`. Pass `NULL` to fall
  back to the regex `grepl("[A-Z0-9]D$", PARAMCD)` pattern, which keeps
  any code whose last two characters are an uppercase letter/digit
  followed by `D`.

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

Named list of data frames (same format as
[`t_pkpt03_col()`](https://pharmaverse.github.io/aNCA/reference/t_pkpt03_col.md)).

## Examples

``` r
if (FALSE) { # \dontrun{
adpp <- export_cdisc(res_nca)$adpp
tables <- t_pkpt07_norm(adpp)
# Include a custom dose-normalized code:
tables <- t_pkpt07_norm(adpp, paramcd_filter = c("CMAXD", "AUCLSTD", "MYPARAMD"))
} # }
```
