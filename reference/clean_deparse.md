# Convert R objects into reproducible R code strings (internal)

This internal S3 generic converts common R objects (data frames, lists,
atomic vectors, etc.) into character strings containing R code that will
reconstruct the object. It is used by the app script generator to
serialize `session$userData` values into a runnable R script.

## Usage

``` r
clean_deparse(obj, indent = 0, max_per_line = 10, min_to_rep = 3)
```

## Arguments

- obj:

  An R object to convert to a string of R code.

- indent:

  Integer indentation level for multi-line outputs.

- max_per_line:

  Maximum number of elements to include per line for long vectors/lists.

- min_to_rep:

  Minimum number of repeated elements to use
  [`rep()`](https://rdrr.io/r/base/rep.html) for long vectors/lists.

## Value

A single string containing R code that, when evaluated, will reconstruct
`obj` (or a close approximation for complex types).
