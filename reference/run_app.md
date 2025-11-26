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

## Value

No return value, called for side effects to launch the Shiny
application.

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
# \donttest{
  # Show the packaged example path (safe non-interactive snippet)
  adnca_path <- system.file("shiny/data/Dummy_data.csv", package = "aNCA")
  adnca_path
#> [1] "/home/runner/work/_temp/Library/aNCA/shiny/data/Dummy_data.csv"

  # To actually launch the app, run interactively:
  if (interactive()) {
    run_app(datapath = adnca_path)
  }
# }
```
