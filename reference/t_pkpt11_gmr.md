# GMR Table with Confidence Intervals (pkpt11)

Computes geometric mean ratios (GMR) with 90% confidence intervals for
selected PK parameters, comparing each treatment arm to a reference arm.

## Usage

``` r
t_pkpt11_gmr(
  data,
  ref_arm = NULL,
  ci_level = 0.9,
  list_vars = c("PPCAT"),
  strat_var = "TRT01A",
  param_var = "PARAM",
  value_var = "AVAL"
)
```

## Arguments

- data:

  A CDISC ADPP data frame.

- ref_arm:

  Character string identifying the reference treatment arm in
  `strat_var`. If `NULL` (default), the first arm in sorted order is
  used.

- ci_level:

  Confidence level for the geometric mean ratio CI. Default: `0.90`.

- list_vars:

  Character vector of columns used to split output into separate tables.
  Default: `c("PPCAT")`. `AVISIT` is a conditional ADPP column that is
  typically absent from `export_cdisc()$adpp`; it is silently skipped
  when not present so there is no need to remove it manually, but adding
  it only helps when your ADPP actually contains visit information.

- strat_var:

  Single treatment-arm column that defines the comparison axis (each arm
  is compared against `ref_arm`). Default: `"TRT01A"`. Unlike the
  summary tables, this must be a single column.

- param_var:

  Column containing parameter names shown as rows. Default: `"PARAM"`.

- value_var:

  Column containing the numeric analysis value. Default: `"AVAL"`.

## Value

Named list of data frames, one per combination of `list_vars`. Each data
frame has columns: `strat_var`, `param_var`, `n_ref`, `n_trt`, `GMR`,
`CI_lower`, `CI_upper`.

## Details

The confidence interval is computed on the log scale using a two-sample
t-test approach: `exp(log_ratio +/- t * SE)` where SE is derived from
the pooled within-group standard deviations on the log scale.

## Examples

``` r
if (FALSE) { # \dontrun{
adpp <- export_cdisc(res_nca)$adpp
tables <- t_pkpt11_gmr(adpp, ref_arm = "Placebo")
} # }
```
