# Validates data table with raw pk data.

Validates data table with raw pk data.

## Usage

``` r
validate_pk(pk_data)
```

## Arguments

- pk_data:

  Object to check.

## Value

Original, unchanged object. If any of the checks do not pass, throws an
error.

## Details

Performs the following checks:

- If provided variable is of class `data.frame`.

- If number of rows in provided data frame is greater than 0.

Throws an error if any of the checks does not pass.
