# Validate CDISC datasets against metadata-declared data types

Checks each column of the supplied CDISC datasets against the expected
storage class and maximum length declared in `metadata_nca_variables`.
This is intentionally not a full CDISC conformance engine: it covers
only what the metadata can assert (class and length), and never fails on
metadata `Type` values it cannot interpret.

## Usage

``` r
validate_cdisc_types(
  cdisc_data,
  metadata = metadata_nca_variables,
  check_length = TRUE,
  grouping_vars = character(0)
)
```

## Arguments

- cdisc_data:

  Named list of CDISC data frames keyed `pp`, `adpp`, and/or `adnca` (as
  returned by
  [`export_cdisc()`](https://pharmaverse.github.io/aNCA/reference/export_cdisc.md)).
  Entries with other names or that are not data frames are ignored.

- metadata:

  Metadata data frame with `Dataset`, `Variable`, `Type`, and `Length`
  columns. Defaults to `metadata_nca_variables`.

- check_length:

  Logical. Check character values against the declared `Length`?
  Defaults to `TRUE`.

- grouping_vars:

  Character vector of explicitly selected grouping variables. Undeclared
  variables in this vector are allowed in ADPP and ADNCA when they
  contain supported atomic vectors, with a warning that metadata
  type/length checks are unavailable. PP remains strict. Known metadata
  rules always apply.

## Value

A tidy findings data frame with columns `Dataset`, `Variable`, `Check`,
`Severity`, `Expected`, `Observed`, `N_Affected`, and `Message`. Zero
rows means the data conforms.

## Details

Indexed CDISC variable families are resolved to their metadata template:
a data column falls back from an exact name match to an index-agnostic
match, so `NCA1XRS`/`NCA2XRS` match the `NCAwXRS` template and
`CRIT2`/`CRIT3FL` match the `CRIT1`/`CRIT1FL` family. An exact metadata
entry always takes precedence over the template.

## Examples

``` r
cdisc_data <- list(
  adnca = data.frame(STUDYID = "S1", AVAL = 1.2, stringsAsFactors = FALSE)
)
validate_cdisc_types(cdisc_data)
#> [1] Dataset    Variable   Check      Severity   Expected   Observed   N_Affected
#> [8] Message   
#> <0 rows> (or 0-length row.names)
```
