# Urine Concentration and Volume Listing (pkcl02)

Filters ADNCA to urine specimen rows (`PCSPEC == "URINE"`, ignoring
case) then delegates to
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

  Retained for compatibility with older scripts; ignored. The listing
  always includes only urine. Non-urine values trigger a warning.

- listgroup_vars:

  Character vector of columns used to split output into separate
  listings. Default: `c("PARAM", "PCSPEC")`.

- displaying_vars:

  Character vector of columns to display. When `NULL` (default), uses
  `c("NFRLT", "AFRLT", "AVAL")` plus `VOLUME`/`VOLUMEU` if those columns
  exist in the data.

- ...:

  Additional arguments forwarded to
  [`l_pkcl01()`](https://pharmaverse.github.io/aNCA/reference/l_pkcl01.md).

## Value

A named list of `listing_df` objects.

## Details

`PCSPEC` is required to identify urine records. Missing specimen values
are excluded. An error explains when `PCSPEC` is absent or no urine
records remain.

## Examples

``` r
if (FALSE) { # \dontrun{
adnca <- export_cdisc(res_nca)$adnca
listings <- l_pkcl02_uri(adnca)
print(listings[[1]])
} # }
```
