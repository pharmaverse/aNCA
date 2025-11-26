# Create C0 Impute Column

Defines an impute column in the intervals of the PKNCAdata object based
on data

## Usage

``` r
create_start_impute(pknca_data)
```

## Arguments

- pknca_data:

  A PKNCAdata object containing concentration and dose data.

## Value

A PKNCAdata object with updated intervals table including start
imputation strategies.

## Examples

``` r
adnca <- read.csv(system.file("shiny/data/Dummy_data.csv", package = "aNCA"))
pknca_data <- PKNCA_create_data_object(adnca)
pknca_data <- create_start_impute(pknca_data)
```
