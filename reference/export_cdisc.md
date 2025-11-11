# Export CDISC Data

This function processes the results from a PKNCA and exports them into
CDISC compliant datasets. Attention: All parameters that do no match
pptest dataframe will be lost in this pipeline!

## Usage

``` r
export_cdisc(res_nca)
```

## Arguments

- res_nca:

  Object with results of the NCA analysis.

## Value

A list with two data frames:

- pp:

  A data frame containing the PP (Pharmacokinetic Parameters) domain
  data.

- adpp:

  A data frame containing the ADPP (Analysis Dataset for Pharmacokinetic
  Parameters) domain data.

## Details

Outputs are the following:

- pknca_result Output from function call `pk.nca()` (formatted)

- pknca_result_raw Output from function call `pk.nca()` (needs to be
  merged with upper later on but now we avoid merge conflict)
