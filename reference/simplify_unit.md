# Simplify compound unit expressions

This function takes a units object or a character string representing a
unit expression and returns a simplified units using the units package
simplifications.

## Usage

``` r
simplify_unit(x, as_character = FALSE)
```

## Arguments

- x:

  A units object, character string, or vector of either to be
  simplified.

- as_character:

  Logical. TRUE returns the result as a character, FALSE (default) as a
  unit object.

## Value

A simplified units object, or a list of units objects if input is a
vector.

## Examples

``` r
# Using a units object
u <- units::set_units(1, "L*g/mg", mode = "standard")
simplify_unit(u)
#> 1000 [L]

# Using a character string
simplify_unit("(mg*L)/(mL)")
#> 1000 [mg]
```
