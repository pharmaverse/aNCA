# Mean Urine PK Parameter Profile Plot (pkpg01)

Computes mean (+/- SD) of a urine PK parameter per treatment arm across
collection intervals and draws a connected line plot. Designed for ADPP
data filtered to `PPSPEC %in% urine_specs` (e.g. cumulative amount
excreted or fraction of dose recovered). Returns one ggplot per unique
combination of `list_vars`.

## Usage

``` r
p_pkpg01_cum(
  data,
  strat_var = "TRT01A",
  param_var = "PARAM",
  value_var = "AVAL",
  urine_specs = c("URINE"),
  paramcd_filter = "RCAMINT",
  time_end_var = "PPENINT",
  list_vars = c("PPCAT"),
  title = "Mean Cumulative Amount Recovered in Urine",
  subtitle = NULL,
  footnote = NULL,
  xlab = NULL,
  ylab = NULL
)

p_pkpg01_per(
  data,
  paramcd_filter = "FREXINT",
  title = "Mean Percentage of Dose Recovered in Urine",
  ylab = "Percent Dose Recovered (%)",
  ...
)
```

## Arguments

- data:

  A CDISC ADPP data frame (from `export_cdisc()$adpp`).

- strat_var:

  Column for treatment arm colour/grouping. Default: `"TRT01A"`.

- param_var:

  Column whose unique values label each x-axis position. Default:
  `"PARAM"`.

- value_var:

  Column containing the numeric analysis value. Default: `"AVAL"`.

- urine_specs:

  Character vector of `PPSPEC` values treated as urine, matched
  case-insensitively. Default: `c("URINE")`.

- paramcd_filter:

  Character vector of `PARAMCD` values to keep. Only rows with a
  matching `PARAMCD` are plotted. Pass `NULL` to skip the filter.
  Default: `"RCAMINT"` (cumulative amount recovered per interval).

- time_end_var:

  Column containing the collection interval end time (ISO 8601 duration
  string or numeric hours). Used as a numeric x-axis when parseable;
  falls back to `param_var` labels otherwise. Default: `"PPENINT"`.

- list_vars:

  Columns used to split output into separate plots. Default:
  `c("PPCAT")`.

- title:

  Optional plot title.

- subtitle:

  Optional plot subtitle.

- footnote:

  Optional footnote / caption.

- xlab:

  X-axis label. Default: `"Collection Interval"`.

- ylab:

  Y-axis label. Defaults to the label attribute of `value_var`.

- ...:

  Additional arguments forwarded to `p_pkpg01_cum()`.

## Value

A named list of ggplot objects.

## Functions

- `p_pkpg01_per()`: Mean percentage of dose recovered in urine (pkpg01
  %). Identical to `p_pkpg01_cum()` but defaults to a % dose recovered
  title and y-axis label.

## Examples

``` r
if (FALSE) { # \dontrun{
adpp <- export_cdisc(res_nca)$adpp
plots <- p_pkpg01_cum(adpp)
plots[[1]]
} # }
```
