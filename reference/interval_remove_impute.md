# Remove specified imputation from the intervals in a PKNCAdata or data.frame (intervals) object.

Remove specified imputation from the intervals in a PKNCAdata or
data.frame (intervals) object.

## Usage

``` r
interval_remove_impute(data, target_impute, ...)
```

## Arguments

- data:

  A PKNCAdata object containing the intervals data frame, or a data
  frame of intervals.

- target_impute:

  A character string specifying the imputation method to remove.

- ...:

  arguments passed to `interval_remove_impute`.

## Value

A modified object with the specified imputations removed from the
targeted intervals.

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
  half.life = c(TRUE, FALSE, TRUE),
  cmax = c(TRUE, TRUE, TRUE),
  impute = c("start_conc0,start_predose", "start_predose", "start_conc0"),
  param = c("Analyte1", "Analyte2", "Analyte1")
)

o_data <- PKNCA::PKNCAdata(o_conc, o_dose, intervals = intervals)

# Apply interval_remove_impute function
o_data <- interval_remove_impute(data = o_data,
                                 target_impute = "start_conc0",
                                 target_params = "half.life",
                                 target_groups = data.frame(param = "Analyte1"))
```
