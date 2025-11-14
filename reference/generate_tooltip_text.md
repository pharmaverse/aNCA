# Generate HTML Tooltip Text

Generate HTML Tooltip Text

## Usage

``` r
generate_tooltip_text(data, labels_df, tooltip_vars, type)
```

## Arguments

- data:

  A data.frame with the source data.

- labels_df:

  A data.frame used by
  [`get_label()`](https://pharmaverse.github.io/aNCA/reference/get_label.md)
  to find variable labels.

- tooltip_vars:

  A character vector of column names to include in the tooltip.

- type:

  A character string specifying the label type for
  [`get_label()`](https://pharmaverse.github.io/aNCA/reference/get_label.md).

## Value

A character vector of formatted HTML tooltip strings.

## Details

Creates a character vector of HTML tooltips for each row of a data
frame, suitable for use with `ggplotly`. The output vector of this
function should be added to original plotting data as a column, which
then can be used as tooltip argument in the plotting function.

## Examples

``` r
# Sample data
my_data <- data.frame(
  USUBJID = c("Subject-01", "Subject-02"),
  DOSE = c(100, 200),
  RESPONSE = c(5.4, 8.1)
  )

my_labels <- data.frame(
  Dataset = "ADNCA",
  Variable = "USUBJID",
  Label = "Unique Subject ID"
  ) # Dummy labels object

vars_to_show <- c("USUBJID", "DOSE", "RESPONSE")

# Generate the tooltip text vector
tooltips <- generate_tooltip_text(my_data, my_labels, vars_to_show, "ADNCA")
my_data$tooltip <- tooltips
```
