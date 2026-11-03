# Boxplot of Primary PK Parameters (pkpg03)

Produces one boxplot per PK parameter with treatment arms on the x-axis.
Returns a named list of ggplot objects, one per `PPCAT` x `PPSPEC`
combination in the ADPP data.

## Usage

``` r
p_pkpg03_boxp(
  data,
  strat_var = "TRT01A",
  param_var = "PARAM",
  value_var = "AVAL",
  list_vars = c("PPCAT", "PPSPEC"),
  all_points = FALSE,
  title = NULL,
  subtitle = NULL,
  footnote = NULL,
  ylab = NULL
)

p_pkpg04_boxp(data, ...)
```

## Arguments

- data:

  A CDISC ADPP data frame (from `export_cdisc()$adpp`).

- strat_var:

  Column used for x-axis grouping (treatment arms). Default: `"TRT01A"`.

- param_var:

  Column whose unique values each become a separate plot. Default:
  `"PARAM"`.

- value_var:

  Column containing the numeric analysis value. Default: `"AVAL"`.

- list_vars:

  Columns used to split output into separate plot pages. Default:
  `c("PPCAT", "PPSPEC")`.

- all_points:

  Logical. When `TRUE`, individual data points are overlaid on the boxes
  (pkpg04 style). Default: `FALSE`.

- title:

  Optional plot title. Supports `$COLUMN` value tokens and `!COLUMN`
  column-label tokens, resolved within each plot's data split.

- subtitle:

  Optional plot subtitle string. When `NULL`, generated from each plot's
  analyte, specimen, visit (when available), and parameter/unit pairs.
  Use `""` to suppress the subtitle. Supports the same annotation tokens
  as `title`.

- footnote:

  Optional footnote string. Supports the same annotation tokens as
  `title`.

- ylab:

  Y-axis label. Defaults to the label attribute of `value_var`.

- ...:

  Additional arguments forwarded to `p_pkpg03_boxp()`.

## Value

A named list of ggplot objects.

## Functions

- `p_pkpg04_boxp()`: Boxplot with all individual data points overlaid
  (pkpg04).

## Examples

``` r
if (FALSE) { # \dontrun{
adpp <- export_cdisc(res_nca)$adpp
plots <- p_pkpg03_boxp(adpp)
plots[[1]]
} # }
```
