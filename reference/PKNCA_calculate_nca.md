# Calculates results for PKNCA analysis.

Calculates results for PKNCA analysis.

## Usage

``` r
PKNCA_calculate_nca(pknca_data)
```

## Arguments

- pknca_data:

  Data object created using PKNCA::PKNCAdata() function.

## Value

Results object with start and end times for each dose, from first dose
and from most recent dose

## Details

This function+ calculates results for PKNCA analysis using
[`PKNCA::pk.nca()`](http://humanpred.github.io/pknca/reference/pk.nca.md).
It then joins the results with the dosing data, to create a full results
data frame with the start and end times for each dose, from first and
most recent dose.

## Examples

``` r
example_data <- data.frame(
  STUDYID = rep("STUDY001", 6),
  PCSPEC = rep("Plasma", 6),
  ROUTE = rep("IV", 6),
  DOSETRT = rep("DrugA", 6),
  USUBJID = rep("SUBJ001", 6),
  ATPTREF = rep(1, 6),
  PARAM = rep("AnalyteA", 6),
  AVAL = c(0, 5, 10, 7, 3, 1),
  AVALU = rep("ng/mL", 6),
  DOSEA = rep(100, 6),
  DOSEU = rep("mg", 6),
  AFRLT = c(0, 1, 2, 3, 4, 6),
  ARRLT = c(0, 1, 2, 3, 4, 6),
  NFRLT = c(0, 1, 2, 3, 4, 6),
  ADOSEDUR = rep(0.5, 6),
  RRLTU = rep("hour", 6)
)

# Create a PKNCAdata object
pknca_data <- PKNCA_create_data_object(example_data)

# Perform NCA calculations
nca_results <- PKNCA_calculate_nca(pknca_data)
```
