# Dose-Proportionality Scatter Plot with Power-Model Regression (pkpg02)

Plots individual AVAL values against dose (DOSEA) on a log-log scale
with one facet per PK parameter. A power-model regression line (log y =
a + b \* log x) is overlaid on each facet together with the slope
estimate and its confidence interval, enabling visual assessment of
dose-proportionality (slope b = 1).

## Usage

``` r
p_pkpg02_doseprop(
  data,
  dose_var = "DOSEA",
  value_var = "AVAL",
  param_var = "PARAM",
  strat_var = "TRT01A",
  list_vars = c("PPCAT", "PPSPEC"),
  ci_level = 0.9,
  log_scale = TRUE,
  title = NULL,
  subtitle = NULL,
  footnote = NULL,
  xlab = NULL,
  ylab = NULL
)
```

## Arguments

- data:

  A CDISC ADPP data frame (from `export_cdisc()$adpp`).

- dose_var:

  Column containing the administered dose. Default: `"DOSEA"`.

- value_var:

  Column containing the PK parameter value. Default: `"AVAL"`.

- param_var:

  Column used as facet variable. Default: `"PARAM"`.

- strat_var:

  Column used for point colour. Default: `"TRT01A"`.

- list_vars:

  Columns used to split output into separate plots. Default:
  `c("PPCAT", "PPSPEC")`.

- ci_level:

  Confidence level for the slope CI. Default: `0.90`.

- log_scale:

  Logical. When `TRUE` (default), both axes are log10-scaled.

- title:

  Optional plot title.

- subtitle:

  Optional plot subtitle.

- footnote:

  Optional footnote / caption.

- xlab:

  X-axis label. Defaults to `dose_var` label + unit.

- ylab:

  Y-axis label. Defaults to the label attribute of `value_var`.

## Value

A named list of ggplot objects.

## Examples

``` r
if (FALSE) { # \dontrun{
adpp <- export_cdisc(res_nca)$adpp
plots <- p_pkpg02_doseprop(adpp)
plots[[1]]
} # }
```
