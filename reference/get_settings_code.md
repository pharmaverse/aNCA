# Generate a session script from settings and mapping files

This function reads a settings yaml file and data path, and generates an
R script that can reproduce the session using a template.

## Usage

``` r
get_settings_code(
  settings_file_path,
  data_path,
  output_path = "settings_code.R",
  template_path = system.file("shiny/www/templates/script_template.R", package = "aNCA"),
  mapping = default_mapping,
  ratio_table = data.frame()
)
```

## Arguments

- settings_file_path:

  Path to the yaml file containing the settings list.

- data_path:

  Path to the data file to be referenced in the script.

- output_path:

  Path to write the resulting script file.

- template_path:

  Path to the R script template file. By default, uses the one installed
  from your aNCA package version.

- mapping:

  Named list mapping variable names (default: `default_mapping`).

- ratio_table:

  Data frame containing ratio definitions (default: empty data frame).

## Value

Invisibly returns the output_path.
