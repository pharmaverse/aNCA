# Create PK Concentration Listing

This function creates a listing of pharmacokinetic (PK) concentration
data segregating a dataset in lists that are customizable in title,
footnotes, grouping/displayed variables, missing/zero values and/or
number of digits displayed.

## Usage

``` r
l_pkcl01(
  data,
  listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
  grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
  displaying_vars = c("NFRLT", "AFRLT", "AVAL"),
  formatting_vars_table = NULL,
  title = paste0("Listing of PK Concentration by Treatment Group,",
    "Subject and Nominal Time, PK Population"),
  subtitle = NULL,
  footnote = "*: Subjects excluded from the summary table and mean plots"
)
```

## Arguments

- data:

  A data frame containing the PK concentration data.

- listgroup_vars:

  A character vector specifying the variables to separate lists.

- grouping_vars:

  A character vector specifying the grouping variables within each list.

- displaying_vars:

  A character vector specifying the variables to display in the listing.

- formatting_vars_table:

  A data frame with the formatting of each variable. See details.

- title:

  A character string to parse specifying the main title for the entire
  listing.

- subtitle:

  A character string to parse specifying the subtitle to use for each
  list.

- footnote:

  A character string to parse specifying the footnote of the listing
  table.

## Value

A list of listings, each corresponding to a unique combination of the
grouping variables.

## Details

The function performs the following steps:

- Groups the data based on the specified grouping variables.

- Formats the 0 and NA values as defined by the formatting table.

- Creates a listing for each unique combination of the grouping
  variables.

The `formatting_vars_table` should be a data frame with the following
columns:

- `var_name`: The name of the variable.

- `Label`: The label for the variable.

- `na_str`: The string to use for NA values.

- `zero_str`: The string to use for 0 values.

- `align`: The alignment for the variable (e.g., "center").

- `format_fun`: The formatting function to use ("round" or "signif").

- `digits`: The number of digits to use for numeric formatting.

## Author

Gerardo Rodriguez

## Examples

``` r
  # Create a sample dataframe 'data' with the required variables
  set.seed(123)
  data <- data.frame(
    PARAM = rep(c("Param1", "Param2"), each = 6),
    PCSPEC = rep(c("Blood", "Urine"), each = 6),
    TRT01A = rep(c("Treatment1", "Treatment2"), each = 6),
    USUBJID = rep(c(rep(1, 3), rep(2, 3)), 2),
    NFRLT = rep(1:3, 4),
    AFRLT = rep(1:3, 4) + runif(12, 0, 0.5),
    TIMEU = "hours",
    AVAL = rep(0:2, 4) + runif(12, 0, 0.5),
    AVALU = "mg/L"
  )

  # Define the formatting table
  formatting_vars_table <- data.frame(var_name = names(data),
                                      Label = c("Parameter", "Specimen", "Treatment Arm",
                                                "Unique Subject ID", "Norminal Time ($TIMEU)",
                                                "Actual Time ($TIMEU)", "Time Unit",
                                                "Analyte Value ($AVALU)", "Analyte Unit"),
                                      na_str = "Missing",
                                      zero_str = c(rep("0", 7), "BLQ", "0"),
                                      align = "center",
                                      format_fun = c(NA, NA, NA, NA,
                                                     "round", "round", NA, "round", NA),
                                      digits = c(NA, NA, NA, NA, 2, 2, NA, 3, NA))

  # Call the l_pkcl01 function with the sample data
  listing_ex <- l_pkcl01(data = data,
                         listgroup_vars = c("PARAM", "PCSPEC"),
                         grouping_vars = c("TRT01A", "USUBJID"),
                         displaying_vars = c("NFRLT", "AFRLT", "AVAL"),
                         formatting_vars_table = formatting_vars_table,
                         title = "Listing of PK Concentration",
                         subtitle = "Subjects with !PARAM: $PARAM (!PCSPEC: $PCSPEC)"
                         )
  print(listing_ex)
#> $Param1.Blood
#> Listing of PK Concentration
#> Subjects with Parameter: Param1 (Specimen: Blood)
#> 
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————
#> Treatment Arm   Unique Subject ID   Norminal Time (hours)   Actual Time (hours)   Analyte Value (mg/L)
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————
#>  Treatment1             1                     1                    1.14                  0.339        
#>                                               2                    2.39                  1.286        
#>                                               3                     3.2                  2.051        
#>                         2                     1                    1.44                   0.45        
#>                                               2                    2.47                  1.123        
#>                                               3                    3.02                  2.021        
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————
#> 
#> *: Subjects excluded from the summary table and mean plots
#> 
#> $Param2.Urine
#> Listing of PK Concentration
#> Subjects with Parameter: Param2 (Specimen: Urine)
#> 
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————
#> Treatment Arm   Unique Subject ID   Norminal Time (hours)   Actual Time (hours)   Analyte Value (mg/L)
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————
#>  Treatment2             1                     1                    1.26                  0.164        
#>                                               2                    2.45                  1.477        
#>                                               3                    3.28                  2.445        
#>                         2                     1                    1.23                  0.346        
#>                                               2                    2.48                   1.32        
#>                                               3                    3.23                  2.497        
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————
#> 
#> *: Subjects excluded from the summary table and mean plots
#> 
```
