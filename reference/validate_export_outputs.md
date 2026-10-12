# Validate exported outputs before saving

Checks every output that the export/Save flow would write, so a save can
be refused before any file is created. It combines two levels of
checking:

## Usage

``` r
validate_export_outputs(
  output,
  obj_names = NULL,
  metadata = metadata_nca_variables,
  grouping_vars = character(0)
)
```

## Arguments

- output:

  The export list as assembled for the Save flow: a named, possibly
  nested list of outputs (plots, tables, and `*_code` strings), with the
  CDISC datasets under a `CDISC` element keyed `pp`/`adpp`/`adnca`.

- obj_names:

  Optional character vector of leaf names that will be written. When
  `NULL` (the default) every leaf is validated.

- metadata:

  Metadata data frame passed to
  [`validate_cdisc_types()`](https://pharmaverse.github.io/aNCA/reference/validate_cdisc_types.md)
  for the CDISC value-level checks. Defaults to
  `metadata_nca_variables`.

- grouping_vars:

  Selected custom grouping variables allowed in ADPP/ADNCA, passed to
  [`validate_cdisc_types()`](https://pharmaverse.github.io/aNCA/reference/validate_cdisc_types.md).
  Known fields and PP remain strictly checked.

## Value

A tidy findings data frame with columns `Output`, `Variable`, `Check`,
`Severity`, `Expected`, `Observed`, and `Message`. `Variable` is `NA`
for object-class findings and holds the column name for CDISC
value-level findings. Zero rows means every output is safe to export.

## Details

- **Object class** for all outputs: plot nodes must be a ggplot or
  plotly, table nodes must be a data frame, and `*_code` nodes must be a
  length-1 character string. Anything the export writer cannot serialise
  is reported as an error.

- **Value level** for the CDISC datasets: the `pp`/`adpp`/`adnca`
  entries under `CDISC` are additionally checked against the
  metadata-declared data types and lengths via
  [`validate_cdisc_types()`](https://pharmaverse.github.io/aNCA/reference/validate_cdisc_types.md).

The walk mirrors the export writer: plain lists are recursed into, and
when `obj_names` is supplied only those named leaves (the ones that will
actually be written) are validated.

## Examples

``` r
output <- list(
  nca_results = list(nca_pkparam = data.frame(AVAL = 1.2)),
  CDISC = list(adnca = data.frame(STUDYID = "S1", AVAL = 1.2))
)
validate_export_outputs(output)
#> [1] Output   Variable Check    Severity Expected Observed Message 
#> <0 rows> (or 0-length row.names)
```
