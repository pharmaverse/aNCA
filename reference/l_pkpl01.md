# Individual PK Parameters Listing (pkpl01)

Creates individual-level listings of PK parameters from ADPP data using
the same engine as
[`l_pkcl01()`](https://pharmaverse.github.io/aNCA/reference/l_pkcl01.md).
Returns one listing per unique combination of `listgroup_vars` (default:
`PPCAT` x `PPSPEC`).

## Usage

``` r
l_pkpl01(
  data,
  listgroup_vars = c("PPCAT", "PPSPEC"),
  grouping_vars = c("TRT01A", "USUBJID"),
  param_var = "PARAM",
  value_var = "AVAL",
  unit_var = "AVALU",
  title = "Listing of Individual PK Parameters",
  subtitle = NULL,
  footnote = NULL
)

l_pkpl01_mp(data, ...)
```

## Arguments

- data:

  A CDISC ADPP data frame (from `export_cdisc()$adpp`).

- listgroup_vars:

  Character vector of columns used to split the output into separate
  listings. Default: `c("PPCAT", "PPSPEC")`.

- grouping_vars:

  Character vector of key/header columns within each listing (shown as
  indented row keys). Default: `c("TRT01A", "USUBJID")`.

- param_var:

  Column whose unique values become display columns after pivoting wide.
  Default: `"PARAM"`.

- value_var:

  Column containing the numeric analysis value. Default: `"AVAL"`.

- unit_var:

  Column containing the parameter unit used to build column headers
  (`"<PARAM> (<unit>)"`). Default: `"AVALU"`.

- title:

  Main listing title string.

- subtitle:

  Per-listing subtitle. Supports `$VAR` / `!VAR` annotation syntax.
  Defaults to the unique values of `listgroup_vars`.

- footnote:

  Footnote string.

- ...:

  Additional arguments forwarded to `l_pkpl01()`.

## Value

A named list of `listing_df` objects (one per `listgroup_vars`
combination), suitable for printing in a Shiny `verbatimTextOutput`.

## Functions

- `l_pkpl01_mp()`: Listing filtered to metabolite rows (pkpl01 M/P).
  Uses the same METABFL -\> PPCAT -\> PARAM fallback as
  [`t_pkpt03_MP_col()`](https://pharmaverse.github.io/aNCA/reference/t_pkpt03_col.md).

## Examples

``` r
if (FALSE) { # \dontrun{
adpp <- export_cdisc(res_nca)$adpp
listings <- l_pkpl01(adpp)
print(listings[[1]])
} # }
```
