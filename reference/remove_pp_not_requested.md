# Filter Out Parameters Not Requested in PKNCA Results (Pivot Version)

This function removes parameters from the PKNCA results that were not
requested by the user, using a pivoted approach that also handles
bioavailability settings.

## Usage

``` r
remove_pp_not_requested(pknca_res)
```

## Arguments

- pknca_res:

  A PKNCA results object containing at least \$data\$intervals and
  \$result.

## Value

The PKNCA results object with non requested parameters removed from
\$result.
