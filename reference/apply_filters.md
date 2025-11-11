# Apply Filters to a Dataset

This function applies a set of filters to a dataset. Each filter
specifies a column, condition, and value to filter the dataset.

## Usage

``` r
apply_filters(data, filters)
```

## Arguments

- data:

  A data frame containing the raw data to be filtered.

- filters:

  A list of filters, where each filter is a list containing the column,
  condition, and value.

## Value

A data frame containing the filtered data.

## Details

The function iterates over the list of filters and applies each filter
to the dataset. The supported conditions are `==`, `>`, `<`, `>=`, `<=`
and `!=`.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example usage:
  data <- mtcars
  filters <- list(
    list(column = "mpg", condition = ">", value = "20"),
    list(column = "cyl", condition = "==", value = "6")
  )
  filtered_data <- apply_filters(data, filters)
} # }
```
