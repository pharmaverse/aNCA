# Helper function create text used to filter data frame

Helper function create text used to filter data frame

## Usage

``` r
.create_filter_expr(boxplotdata, varvalstofilter)
```

## Arguments

- boxplotdata:

  Data frame to be filtered

- varvalstofilter:

  Character vector specifying which variable and value to pre-filter as
  `colname: value`. By default is NULL (no pre-filtering)

## Value

The filter expression
