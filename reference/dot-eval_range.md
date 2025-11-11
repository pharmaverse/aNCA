# Evaluates range notation. If provided notation is invalid, returns NA.

Evaluates range notation. If provided notation is invalid, returns NA.

## Usage

``` r
.eval_range(x)
```

## Arguments

- x:

  character string with range notation, e.g. 1:5.

## Value

numeric vector with specified range of numbers, NA if notation is
invalid

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
.eval_range("1:5") # c(1, 2, 3, 4, 5)
.eval_range("5,3:1,15") # c(5, 3, 2, 1, 15)
} # }
```
