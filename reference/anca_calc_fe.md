# Compatibility alias for PKNCA fraction excreted

This non-dotted alias lets older PKNCA releases resolve the calculation
callback while aNCA is attached. Prefer
[`PKNCA::pk.calc.fe()`](https://humanpred.github.io/pknca/reference/pk.calc.fe.html)
for direct calls.

## Usage

``` r
anca_calc_fe(ae, dose)
```

## Arguments

- ae:

  The amount excreted (as a numeric scalar or vector)

- dose:

  The dose (as a numeric scalar or vector)

## Value

Fraction of the dose excreted, as returned by
[`PKNCA::pk.calc.fe()`](https://humanpred.github.io/pknca/reference/pk.calc.fe.html).
