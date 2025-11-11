# Calculate Matrix Ratios This function calculates the ratios for a given data set, based on the shared time points for each matrix concentration sample. The user can input multiple tissues for which ratios should be calculated.

The ratios are calculated as specimen1 / specimen 2.

## Usage

``` r
multiple_matrix_ratios(
  data,
  matrix_col,
  conc_col,
  units_col,
  groups = c("NFRLT", "USUBJID"),
  spec1,
  spec2
)
```

## Arguments

- data:

  A data frame containing the concentration data.

- matrix_col:

  A character string specifying the column name for the matrix type.

- conc_col:

  A character string specifying the column name for the concentration
  data.

- units_col:

  A character string specifying the column name for the units.

- groups:

  A character vector of grouping variables to use for the analysis. Must
  include time column, USUBJID, and optionally, other grouping
  variables.

- spec1:

  A character string specifying the value for the first specimen type(s)
  in the matrix_col.

- spec2:

  A character string specifying the value for the second specimen
  type(s) in the matrix_col.

## Value

A data frame containing the ratios.

## Examples

``` r
data <- data.frame(
  USUBJID = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
  NFRLT = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2),
  MATRIX = c(
    "BLOOD", "BLOOD", "BLOOD", "PLASMA", "PLASMA", "PLASMA",
    "BRAIN", "BRAIN", "BRAIN", "LIVER", "LIVER", "LIVER"
  ),
  CONC = c(10, 20, 15, 25, 30, 40, 5, 10, 8, 12, 18, 16),
  UNITS = rep("ng/mL", 12)
)
multiple_matrix_ratios(data, "MATRIX", "CONC", "UNITS", c("NFRLT", "USUBJID"), "BLOOD", "PLASMA")
#> # A tibble: 3 × 8
#> # Rowwise: 
#>   NFRLT USUBJID Ratio_Type Spec1_Value Spec1_Units Spec2_Value Spec2_Units Ratio
#>   <dbl> <chr>   <chr>            <dbl> <chr>             <dbl> <chr>       <dbl>
#> 1     0 A       BLOOD/PLA…          10 ng/mL                25 ng/mL       0.4  
#> 2     1 A       BLOOD/PLA…          20 ng/mL                30 ng/mL       0.667
#> 3     2 A       BLOOD/PLA…          15 ng/mL                40 ng/mL       0.375
```
