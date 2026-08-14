# Urine Concentration and Volume Listing (pkcl02)

Filters ADNCA to urine specimen rows (where `PCSPEC %in% urine_specs`)
then delegates to
[`l_pkcl01()`](https://pharmaverse.github.io/aNCA/reference/l_pkcl01.md)
with VOLUME and VOLUMEU added to the displayed columns when those
columns are present in the data.

## Usage

``` r
l_pkcl02_uri(
  data,
  urine_specs = c("URINE"),
  listgroup_vars = c("PARAM", "PCSPEC"),
  displaying_vars = NULL,
  ...
)
```

## Arguments

- data:

  A CDISC ADNCA data frame (from `export_cdisc()$adnca`).

- urine_specs:

  Character vector of specimen type values to keep, matched
  case-insensitively against `PCSPEC`. Default: `c("URINE")`.

- listgroup_vars:

  Character vector of columns used to split output into separate
  listings. Default: `c("PARAM", "PCSPEC")`. When `PCSPEC` is absent
  from `data`, it is silently removed from this vector.

- displaying_vars:

  Character vector of columns to display. When `NULL` (default), uses
  `c("NFRLT", "AFRLT", "AVAL")` plus `VOLUME`/`VOLUMEU` if those columns
  exist in the data.

- ...:

  Additional arguments forwarded to
  [`l_pkcl01()`](https://pharmaverse.github.io/aNCA/reference/l_pkcl01.md).

## Value

A named list of `listing_df` objects.

## Examples

``` r
if (FALSE) { # \dontrun{
adnca <- export_cdisc(res_nca)$adnca
listings <- l_pkcl02_uri(adnca)
print(listings[[1]])
} # }
```
