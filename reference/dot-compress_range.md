# Compresses a numeric vector into the simplest possible character string that, when evaluated, will create the same numeric vector.

Compresses a numeric vector into the simplest possible character string
that, when evaluated, will create the same numeric vector.

## Usage

``` r
.compress_range(range_vector)
```

## Arguments

- range_vector:

  numeric vector with numbers to compress into string

## Value

simplest possible character string representing provided vector

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
.compress_range(c(1, 2, 3, 4)) # "1:4"
.compress_range(c(15, 1, 11, 4, 5, 10, 2, 12, 3)) # "1:5,10:12,15"
} # }
```
