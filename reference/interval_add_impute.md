# Add specified imputation methods to the intervals in a PKNCAdata or data.frame object.

Add specified imputation methods to the intervals in a PKNCAdata or
data.frame object.

## Usage

``` r
interval_add_impute(
  data,
  target_impute,
  after,
  target_params,
  target_groups,
  ...
)
```

## Arguments

- data:

  A PKNCAdata object containing the intervals data frame, or a data
  frame of intervals.

- target_impute:

  A character string specifying the imputation method to be added.

- after:

  Numeric value specifying the index in which the imputation will be
  added (optional). First is 0, last Inf. If missing, the imputation
  method is added at the end (Inf).

- target_params:

  A character vector specifying the parameters to be targeted
  (optional). If missing, all TRUE in the intervals are taken.

- target_groups:

  A data frame specifying the intervals to be targeted (optional). If
  missing, all relevant groups are considered.

- ...:

  arguments passed to `interval_add_impute`.

## Value

A modified PKNCAdata object with specified imputation methods on the
target intervals.

## Details

If already present the target_impute method will be added substituting
the existing one. All new intervals created will be added right after
their original ones.

## Examples

``` r
d_conc <- data.frame(
  conc = c(1, 0.6, 0.2, 0.1, 0.9, 0.4, 1.2, 0.8, 0.3, 0.2, 1.1, 0.5),
  time = rep(0:5, 2),
  ID = rep(1:2, each = 6),
  param = rep(c("Analyte1", "Analyte2"), each = 6)
)

d_dose <- data.frame(
  dose = c(100, 200),
  time = c(0, 0),
  ID = c(1, 2)
)

o_conc <- PKNCA::PKNCAconc(d_conc, conc ~ time | ID / param)
o_dose <- PKNCA::PKNCAdose(d_dose, dose ~ time | ID)

intervals <- data.frame(
  start = c(0, 0, 0),
  end = c(3, 5, Inf),
  half.life = c(TRUE, TRUE, TRUE),
  cmax = c(TRUE, TRUE, TRUE),
  impute = c("start_conc0,start_predose", "start_predose", "start_conc0"),
  param = c("Analyte1", "Analyte2", "Analyte1")
)

o_data <- PKNCA::PKNCAdata(o_conc, o_dose, intervals = intervals)

# Apply interval_add_impute function
o_data <- interval_add_impute(o_data,
                              target_impute = "start_conc0",
                              target_params = "half.life",
                              target_groups = data.frame(param = "Analyte1"))
```
