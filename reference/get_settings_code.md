# Generate a session script from a settings YAML file

Reads a settings YAML file and generates an R script that can reproduce
the session. Mapping, filters, ratio table, and units are all read from
the YAML.

## Usage

``` r
get_settings_code(
  settings_file_path,
  data_path,
  output_path = "settings_code.R",
  template_path = system.file("www/templates/script_template.R", package = "aNCA")
)
```

## Arguments

- settings_file_path:

  Path to the YAML file containing the settings.

- data_path:

  Path to the data file to be referenced in the script.

- output_path:

  Path to write the resulting script file.

- template_path:

  Path to the R script template file. By default, uses the one installed
  from your aNCA package version.

## Value

Invisibly returns the output_path.
