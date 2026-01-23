# Generate a session script code in R that can replicate the App outputs

Generate a session script code in R that can replicate the App outputs

## Usage

``` r
get_session_code(template_path, session, output_path)
```

## Arguments

- template_path:

  Path to the R script template (e.g., script_template.R)

- session:

  The session object containing userData, etc.

- output_path:

  Path to write the resulting script file (e.g., "output_script.R")

## Value

The output_path (invisibly)
