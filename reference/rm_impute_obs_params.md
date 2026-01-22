# Apply Imputation for PKNCA Data

Applies imputation rules to a PKNCAdata object, imputing start values
and then selectively removing imputation for parameters that are not
dependent on AUC.

## Usage

``` r
rm_impute_obs_params(data, metadata_nca_parameters = metadata_nca_parameters)
```

## Arguments

- data:

  A PKNCAdata object.

- metadata_nca_parameters:

  A data frame mapping PKNCA parameters (`PKNCA`) and information on
  their parameter dependencies ('Depends').

## Value

A PKNCAdata object with imputation rules applied.
