# Build Units Table for PKNCA

This function generates a PKNCA units table including the potential unit
segregating columns among the dose and/or concentration groups.

## Usage

``` r
PKNCA_build_units_table(o_conc, o_dose)
```

## Arguments

- o_conc:

  A PKNCA concentration object (PKNCAconc).

- o_dose:

  A PKNCA dose object (PKNCAdose).

## Value

A data frame containing the PKNCA formatted units table.

## Details

The function performs the following steps:

1.  Ensures the unit columns (e.g., `concu`, `timeu`, `doseu`,
    `amountu`) exist in the inputs.

2.  Joins the concentration and dose data based on their grouping
    columns.

3.  Generates a PKNCA units table for each group, including conversion
    factors and custom units.

4.  Returns a unique table with relevant columns for PKNCA analysis.

## Examples

``` r
# Assuming `o_conc` and `o_dose` are valid PKNCA objects:
# 1) Sharing group variables in their formulas
# 2) Time units are the same within dose groups
# 3) Units are the same for subjects within the same concentration group

d_conc <- data.frame(
  subj = 1,
  analyte = rep(c("A", "B"), each = 2),
  concu = rep(c("ng/mL", "ug/mL"), each = 2),
  conc = c(0, 2, 0, 5),
  time = rep(0:1, 2),
  timeu = "h"
)
d_dose <- data.frame(
  subj = 1,
  dose = 100,
  doseu = "mg",
  time = 0,
  timeu = "h"
)
o_conc <- PKNCA::PKNCAconc(d_conc, conc ~ time | subj / analyte, concu = "concu")
o_dose <- PKNCA::PKNCAdose(d_dose, dose ~ time | subj, doseu = "doseu")
units_table <- PKNCA_build_units_table(o_conc, o_dose)
```
