# Boxplot of Metabolite/Parent PK Parameter Ratios (pkpg06)

Filters ADPP to metabolite rows using the same fallback logic as
[`t_pkpt03_MP_col()`](https://pharmaverse.github.io/aNCA/reference/t_pkpt03_col.md)
(METABFL preferred, then PPCAT/PARAM grep for "metab"), then delegates
to
[`p_pkpg03_boxp()`](https://pharmaverse.github.io/aNCA/reference/p_pkpg03_boxp.md).

## Usage

``` r
p_pkpg06_mp(data, ...)
```

## Arguments

- data:

  A CDISC ADPP data frame (from `export_cdisc()$adpp`).

- ...:

  Additional arguments forwarded to
  [`p_pkpg03_boxp()`](https://pharmaverse.github.io/aNCA/reference/p_pkpg03_boxp.md).

## Value

A named list of ggplot objects (same format as
[`p_pkpg03_boxp()`](https://pharmaverse.github.io/aNCA/reference/p_pkpg03_boxp.md)).

## Examples

``` r
if (FALSE) { # \dontrun{
adpp <- export_cdisc(res_nca)$adpp
plots <- p_pkpg06_mp(adpp)
plots[[1]]
} # }
```
