# Create METABFL Column Based on Selected Metabolites

Create METABFL Column Based on Selected Metabolites

## Usage

``` r
create_metabfl(dataset, metabolites)
```

## Arguments

- dataset:

  A data frame containing a dataset with a PARAM (parameter) column.

- metabolites:

  A character vector of parameter names representing metabolites.

## Value

The input dataset with an additional METABFL column indicating
metabolite records ("Y") or non-metabolite records ("").
