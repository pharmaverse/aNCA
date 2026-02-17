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
imputation strategies. If the intervals are ambiguous and can refer to
multiple concentration groups it will define them to choose the proper
imputation for each.

## Examples

``` r
adnca <- read.csv(system.file("shiny/data/example-ADNCA.csv", package = "aNCA"))
pknca_data <- PKNCA_create_data_object(adnca)
pknca_data <- create_start_impute(pknca_data)
```
