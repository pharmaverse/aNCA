# Add bioavailability results to PKNCA results

Add bioavailability results to PKNCA results

## Usage

``` r
add_f_to_pknca_results(res_nca, f_aucs)
```

## Arguments

- res_nca:

  A list containing non-compartmental analysis (NCA) results, including
  concentration and dose data.

- f_aucs:

  A character vector of the comparing AUC parameter/s including the
  prefix f\_ (e.g., `c("f_aucinf.obs", "f_auclast")`).

## Value

A PKNCA result object which results data frame contains added the
bioavailability calculations requested based on the AUCs provided in
`f_aucs`.
