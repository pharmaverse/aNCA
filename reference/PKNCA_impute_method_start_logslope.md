# This function imputes the start concentration using the log slope method.

This function imputes the start concentration using the log slope
method.

## Usage

``` r
PKNCA_impute_method_start_logslope(
  conc,
  time,
  start,
  end,
  ...,
  options = list()
)
```

## Arguments

- conc:

  Numeric vector of concentrations.

- time:

  Numeric vector of times corresponding to the concentrations.

- start:

  Numeric value indicating the start/dose time.

- end:

  Numeric value indicating the end time.

- ...:

  Additional arguments (currently not used).

- options:

  List of options (currently not used).

## Value

A data frame with imputed start concentration.

## Details

This function adheres to the structure required by the `PKNCA` package
to work with its functionalities. For more information, see the [PKNCA
Data Imputation Vignette](https://CRAN.R-project.org/package=PKNCA).

## Examples

``` r
conc <- c(5, 4, 3, 2, 1)
time <- c(1, 2, 3, 4, 5)
start <- 0
end <- 4
PKNCA_impute_method_start_logslope(conc, time, start, end)
#>   conc time
#> 6 6.25    0
#> 1 5.00    1
#> 2 4.00    2
#> 3 3.00    3
#> 4 2.00    4
#> 5 1.00    5
```
