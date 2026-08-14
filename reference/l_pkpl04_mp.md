# Individual Treatment Comparison Listing (pkpl04)

Produces a per-subject listing of individual PK parameter values
organised for treatment comparison. Each listing page covers one
PPCAT/PPSPEC combination. PK parameters become display columns (one
column per PARAM value) and the rows are keyed by `TRT01A` and
`USUBJID`.

## Usage

``` r
l_pkpl04_mp(
  data,
  grouping_vars = c("PARAM", "TRT01A", "USUBJID"),
  title = "Listing of Individual PK Parameter Values by Treatment",
  ...
)
```

## Arguments

- data:

  A CDISC ADPP data frame (from `export_cdisc()$adpp`).

- grouping_vars:

  Columns used to identify row keys before pivoting. `PARAM` must be
  included so it is spread into display columns. Default:
  `c("PARAM", "TRT01A", "USUBJID")`.

- title:

  Main listing title string.

- ...:

  Additional arguments forwarded to
  [`l_pkpl01()`](https://pharmaverse.github.io/aNCA/reference/l_pkpl01.md).

## Value

A named list of `listing_df` objects.

## Details

This listing shows the raw individual `AVAL` values from ADPP, not
pre-computed ratios. If your ADPP contains NCA ratio parameters (e.g.
metabolite-to-parent AUC ratios added via the aNCA ratio-calculation
module), those parameters are displayed here just like any other PARAM
row. The `_mp` suffix in the function name reflects its typical use with
metabolite/parent ratio parameters, but no metabolite filtering is
applied – all PARAM values in the data are included.

Note: `PARAM` is listed in `grouping_vars` so that it participates in
the `pivot_wider` step (each unique PARAM value becomes a column
header). After pivoting, `PARAM` is no longer a row-key column; the
actual listing keys are `TRT01A` and `USUBJID`.

## Examples

``` r
if (FALSE) { # \dontrun{
adpp <- export_cdisc(res_nca)$adpp
listings <- l_pkpl04_mp(adpp)
print(listings[[1]])
} # }
```
