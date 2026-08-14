# Summary Concentration Table (pkct01)

Summarizes PK concentration data by treatment/dose group and nominal
timepoint. Returns one data frame per analyte/specimen combination
containing descriptive statistics across subjects at each scheduled
timepoint.

## Usage

``` r
t_pkct01(
  data,
  list_vars = c("PARAM", "PCSPEC"),
  strat_var = c("TRT01A", "ATPTREF", "NFRLT"),
  value_var = "AVAL",
  blq_var = "AVALC",
  time_var = "NFRLT",
  time_filter = NULL,
  col_group_var = NULL,
  stats = NULL
)

t_pkct01_dose(data, strat_var = c("DOSEA", "ATPTREF", "NFRLT"), ...)

t_pkct01_tad(data, strat_var = c("TRT01A", "ATPTREF", "NRRLT"), ...)

t_pkct01_dose_tad(data, strat_var = c("DOSEA", "ATPTREF", "NRRLT"), ...)
```

## Arguments

- data:

  A CDISC ADNCA data frame (from `export_cdisc()$adnca`).

- list_vars:

  Character vector of columns used to split the output into separate
  tables. Default: `c("PARAM", "PCSPEC")`.

- strat_var:

  One or more columns whose combination defines the table rows
  (stratification). Default: `c("TRT01A", "ATPTREF", "NFRLT")` –
  treatment arm, visit reference, and nominal timepoint. Add or remove
  columns to change how the rows are grouped. Any variable that is also
  a `list_vars` (table-split) column is dropped from the rows, since it
  is constant within each split.

- value_var:

  Column containing the numeric analysis value. Default: `"AVAL"`.

- blq_var:

  Column containing the character analysis value used to detect BLQ
  records. Default: `"AVALC"`. Records where this column equals `"BLQ"`
  are counted separately and excluded from numeric summaries. When
  `blq_var` is absent (as in `export_cdisc()$adnca`, which does not
  include `AVALC`), BLQ is detected via `value_var == 0`, consistent
  with the package convention for post-imputation BLQ encoding.

- time_var:

  Column that the `time_filter` applies to (the nominal timepoint
  column). Default: `"NFRLT"`. Row grouping is controlled entirely by
  `strat_var`; this argument only names the column that `time_filter`
  subsets.

- time_filter:

  Optional vector of `time_var` values to keep. `NULL` (default) keeps
  every timepoint.

- col_group_var:

  Optional subject-level column (e.g. `"SEX"`, `"RACE"`) whose values
  become side-by-side comparison column groups: the full statistic block
  is repeated once per level, nested under a group header. `NULL`
  (default) produces the standard flat table. Must differ from
  `strat_var` and the `list_vars`.

- stats:

  Optional character vector of statistics to display, chosen from
  `c("n", "n_blq", "Mean", "SD", "CV_pct", "Median", "GeoMean", "GeoCV_pct", "Min", "Max")`.
  `NULL` (default) shows all of them.

- ...:

  Additional arguments forwarded to `t_pkct01()`.

## Value

A named list of data frames, one per unique combination of `list_vars`.
Each data frame has one column per `strat_var` followed by the
statistics: `n`, `n_blq`, `Mean`, `SD`, `CV_pct`, `Median`, `GeoMean`,
`GeoCV_pct`, `Min`, `Max`. When `col_group_var` is set, the statistic
columns are prefixed per group level and a `col_groups` attribute drives
the rendered two-level header.

## Details

BLQ values are excluded from all numeric statistics and counted in
`n_blq`. When `blq_var` is present, BLQ is identified as
`df[[blq_var]] == "BLQ"`. When `blq_var` is absent, `value_var == 0` is
used as the fallback BLQ indicator. `GeoMean` is computed on positive
`value_var` values only.

## Functions

- `t_pkct01_dose()`: Stratify by dose instead of treatment arm (first
  dose).

- `t_pkct01_tad()`: Summarize using time after dose (TAD) nominal time.

- `t_pkct01_dose_tad()`: Stratify by dose using TAD nominal time.

## Examples

``` r
if (FALSE) { # \dontrun{
adnca <- export_cdisc(res_nca)$adnca
tables <- t_pkct01(adnca)
tables[[1]]
} # }
```
