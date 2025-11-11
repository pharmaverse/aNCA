# Create a new Quarto presentation file with YAML header and setup chunk

Used internally to initialize a Quarto document for exporting plots and
tables.

## Usage

``` r
create_qmd_doc(
  quarto_path,
  title = "NCA Report",
  libraries = c("plotly", "flextable", "dplyr"),
  rda_path = NULL,
  template = NULL,
  extra_setup = NULL
)
```

## Arguments

- quarto_path:

  Path to the Quarto (.qmd) file to create.

- title:

  Title for the presentation.

- libraries:

  Character vector of libraries to load in setup chunk.

- rda_path:

  Path to the RDS file to be loaded in the document.

- template:

  (Optional) Path to a Quarto template to use (default: NULL).

- extra_setup:

  (Optional) Character vector of extra setup lines to include after
  YAML.

## Value

Invisibly returns TRUE if the file was created.
