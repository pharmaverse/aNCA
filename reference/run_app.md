# Run the Shiny app

Run the Shiny app

## Usage

``` r
run_app(datapath = NULL, ...)
```

## Arguments

- datapath:

  Full path to a single `.csv` or `.rds` data file.

- ...:

  Arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)

## Details

If a `datapath` is provided, the app will attempt to automatically load
the specified dataset on startup. This is achieved by setting an
internal option (`options(aNCA.datapath = datapath)`), which the app
then reads. **This pre-loaded dataset can be overwritten; if a new file
is uploaded using the widget within the app, it will replace the initial
data for the current session.**

If `datapath` is `NULL` (default), the app will launch without
pre-loading any data, and a file must be uploaded manually within the
app.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Create a dummy data file
  temp_data_path <- tempfile(fileext = ".csv")
  write.csv(data.frame(USUBJID = 1, AFRLT = 0:4, AVAL = c(0, 10, 8, 4, 1)),
            file = temp_data_path, row.names = FALSE)

  # Run the app, automatically loading the dummy data
  run_app(datapath = temp_data_path)

  # Run the app without pre-loading data (standard usage)
  run_app()
} # }
```
