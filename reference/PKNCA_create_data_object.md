# Creates a `PKNCA::PKNCAdata` object.

Creates a
[`PKNCA::PKNCAdata`](http://humanpred.github.io/pknca/reference/PKNCAdata.md)
object.

## Usage

``` r
PKNCA_create_data_object(adnca_data)
```

## Arguments

- adnca_data:

  Data table containing ADNCA data.

## Value

`PKNCAdata` object with concentration, doses, and units based on ADNCA
data.

## Details

This function creates a standard PKNCAdata object from ADNCA data. It
requires the following columns in the ADNCA data:

- STUDYID: Study identifier.

- PCSPEC: Matrix.

- ROUTE: Route of administration.

- DOSETRT: Drug identifier.

- USUBJID: Unique subject identifier.

- ATPTREF: (Non- standard column). Can be any column, used for filtering
  the data for NCA

- PARAM: Analyte.

- AVAL: Analysis value.

- AVALU: AVAL unit.

- DOSEA: Dose amount.

- DOSEU: Dose unit.

- AFRLT: Actual time from first dose.

- ARRLT: Actual time from reference dose.

- NFRLT: Nominal time from first dose.

- ADOSEDUR: Duration of dose.

- RRLTU: Time unit.

1.  Creating pk concentration data using
    [`format_pkncaconc_data()`](https://pharmaverse.github.io/aNCA/reference/format_pkncaconc_data.md).

2.  Creating dosing data using
    [`format_pkncadose_data()`](https://pharmaverse.github.io/aNCA/reference/format_pkncadose_data.md).

3.  Creating `PKNCAconc` object using
    [`PKNCA::PKNCAconc()`](http://humanpred.github.io/pknca/reference/PKNCAconc.md).
    with formula
    `AVAL ~ AFRLT | STUDYID + PCSPEC + DOSETRT + USUBJID / PARAM`.

4.  Creating PKNCAdose object using
    [`PKNCA::PKNCAdose()`](http://humanpred.github.io/pknca/reference/PKNCAdose.md).
    with formula `DOSEA ~ AFRLT | STUDYID + DOSETRT + USUBJID`.

5.  Creating PKNCAdata object using
    [`PKNCA::PKNCAdata()`](http://humanpred.github.io/pknca/reference/PKNCAdata.md).

6.  Updating units in PKNCAdata object so each analyte has its own unit.

## Examples

``` r
adnca_data <- data.frame(
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
PKNCA_create_data_object(adnca_data)
#> Formula for concentration:
#>  AVAL ~ AFRLT | STUDYID + PCSPEC + DOSETRT + USUBJID/PARAM
#> <environment: 0x563cc015ce88>
#> Data are dense PK.
#> With 1 subjects defined in the 'USUBJID' column.
#> Nominal time column is: NFRLT
#> 
#> Data for concentration:
#>   STUDYID PCSPEC ROUTE DOSETRT USUBJID ATPTREF    PARAM AVAL AVALU DOSEA DOSEU
#>  STUDY001 Plasma    IV   DrugA SUBJ001       1 AnalyteA    0 ng/mL   100    mg
#>  STUDY001 Plasma    IV   DrugA SUBJ001       1 AnalyteA    5 ng/mL   100    mg
#>  STUDY001 Plasma    IV   DrugA SUBJ001       1 AnalyteA   10 ng/mL   100    mg
#>  STUDY001 Plasma    IV   DrugA SUBJ001       1 AnalyteA    7 ng/mL   100    mg
#>  STUDY001 Plasma    IV   DrugA SUBJ001       1 AnalyteA    3 ng/mL   100    mg
#>  STUDY001 Plasma    IV   DrugA SUBJ001       1 AnalyteA    1 ng/mL   100    mg
#>  AFRLT ARRLT NFRLT ADOSEDUR RRLTU     std_route DOSNOA is.excluded.hl
#>      0     0     0      0.5  hour intravascular      1          FALSE
#>      1     1     1      0.5  hour intravascular      1          FALSE
#>      2     2     2      0.5  hour intravascular      1          FALSE
#>      3     3     3      0.5  hour intravascular      1          FALSE
#>      4     4     4      0.5  hour intravascular      1          FALSE
#>      6     6     6      0.5  hour intravascular      1          FALSE
#>  is.included.hl REASON exclude_half.life exclude volume duration
#>           FALSE     NA             FALSE    <NA>     NA        0
#>           FALSE     NA             FALSE    <NA>     NA        0
#>           FALSE     NA             FALSE    <NA>     NA        0
#>           FALSE     NA             FALSE    <NA>     NA        0
#>           FALSE     NA             FALSE    <NA>     NA        0
#>           FALSE     NA             FALSE    <NA>     NA        0
#>  include_half.life
#>                 NA
#>                 NA
#>                 NA
#>                 NA
#>                 NA
#>                 NA
#> Formula for dosing:
#>  DOSEA ~ AFRLT | STUDYID + DOSETRT + USUBJID
#> Nominal time column is: NFRLT
#> 
#> Data for dosing:
#>   STUDYID PCSPEC ROUTE DOSETRT USUBJID ATPTREF    PARAM AVAL AVALU DOSEA DOSEU
#>  STUDY001 Plasma    IV   DrugA SUBJ001       1 AnalyteA    0 ng/mL   100    mg
#>  AFRLT ARRLT NFRLT ADOSEDUR RRLTU     std_route DOSNOA is.excluded.hl
#>      0     0     0      0.5  hour intravascular      1          FALSE
#>  is.included.hl REASON exclude_half.life exclude
#>           FALSE     NA             FALSE    <NA>
#> 
#> With 1 rows of interval specifications.
#> With units
#> With imputation: NA
#> No options are set differently than default.
```
