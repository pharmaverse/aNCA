# This function imputes the start concentration using the first concentration after dose

This function imputes the start concentration using the first
concentration after dose

## Usage

``` r
PKNCA_impute_method_start_c1(conc, time, start, end, ..., options = list())
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
to work with its functionalities.For more information, see the [PKNCA
Data Imputation Vignette](https://CRAN.R-project.org/package=PKNCA).

## Examples

``` r
conc <- c(1, 2, 3, 4, 5)
time <- c(1, 2, 3, 4, 5)
start <- 0
end <- 4
PKNCA_impute_method_start_c1(conc, time, start, end)
#>   conc time
#> 6    1    0
#> 1    1    1
#> 2    2    2
#> 3    3    3
#> 4    4    4
#> 5    5    5
```
