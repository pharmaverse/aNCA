---
applyTo: "R/**/*.R"
---

# R Package Code

## File naming

- One main function per file: `R/my_function.R`
- Match file name to function: `positive_mean()` → `positive_mean.R`
- Utilities: `R/utils.R` for small helpers

## Dependencies

- Check `DESCRIPTION` Imports before using any package
- For new dependencies: `usethis::use_package("pkg")`
- Use explicit namespaces in code: `dplyr::filter(x > 0)` not `filter(x > 0)`
- No `library()` or `require()` in package code

## Documentation (roxygen2)

All exported functions must have `@param`, `@returns`, and `@export`:

```r
#' Calculate mean of positive values
#'
#' @param x A numeric vector.
#' @param na.rm Logical. Remove NA values? Default `TRUE`.
#'
#' @returns A single numeric value.
#' @export
#'
#' @examples
#' positive_mean(c(-1, 2, 3, NA))
positive_mean <- function(x, na.rm = TRUE) {
  x <- x[x > 0]
  mean(x, na.rm = na.rm)
}
```

Run `devtools::document()` after modifying roxygen comments.

## Global variables (`R/zzz.R`)

Use for non-standard evaluation (NSE) column references:

```r
utils::globalVariables(c("DOSEA", "TRT01A", "GROUP"))
```

- Only for code in `R/`, not in `inst/shiny/`
- Keep alphabetically sorted, no duplicates

## Anti-patterns

- Do not edit `man/` or `NAMESPACE` files manually
- Do not add unused globalVariables to `R/zzz.R`
- Do not push without running `devtools::document()` first
