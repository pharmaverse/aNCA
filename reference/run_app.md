# Run the Shiny app

Run the Shiny app

## Usage

``` r
run_app(datapath = NULL, settings = NULL, settings_version = 1L, ...)
```

## Arguments

- datapath:

  Full path to a single `.csv` or `.rds` data file.

- settings:

  Full path to a `.yaml` settings file.

- settings_version:

  Version selector for versioned settings files. Either an integer index
  (1 = most recent, 2 = second, etc.) or a character string matched
  against the version `comment` field. Defaults to `1L` (most recent).
  Ignored for non-versioned files.

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

If a `settings` path is provided, the app will load the settings file on
startup and apply the saved configuration (analyte, method, units,
etc.). The pre-loaded settings can be overwritten by uploading a new
settings file.

For versioned settings files (containing `current` and `previous`
entries), the most recent version is loaded by default. Use
`settings_version` to select a different version by index or comment:

    run_app(settings = "settings.yaml", settings_version = 2)
    run_app(settings = "settings.yaml", settings_version = "NCA draft")

## Examples

``` r
if (FALSE) { # \dontrun{
  # Launch the app without pre-loaded data or settings:
  run_app()

  # Pre-load a settings file (use the full path to your YAML file):
  run_app(settings = system.file("www/templates/clinical_template.yaml", package = "aNCA"))

  # Pre-load both data and settings:
  run_app(
    datapath = "/path/to/adpc.csv",
    settings = "/path/to/settings.yaml"
  )

  # Load a specific version from a versioned settings file:
  run_app(settings = "/path/to/settings.yaml", settings_version = 2)
  run_app(settings = "/path/to/settings.yaml", settings_version = "NCA draft")
} # }
```
