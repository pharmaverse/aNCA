# Apply Imputation for PKNCA Data

Applies imputation rules to a PKNCAdata object, imputing start values
and then selectively removing imputation for parameters that are not
dependent on AUC.

## Usage

``` r
apply_imputation(data, nca_parameters = metadata_nca_parameters)
```

## Arguments

- data:

  A PKNCAdata object.

- nca_parameters:

  A dataset containing the mapping between PKNCA terms and CDISC terms.

## Value

A PKNCAdata object with imputation rules applied.
