# Create all slides for dose escalation results in a Quarto document

Used internally to generate main and individual slides for each dose
group.

## Usage

``` r
create_qmd_dose_slides(res_dose_slides, quarto_path, title, use_plotly = TRUE)
```

## Arguments

- res_dose_slides:

  List of results for each dose group.

- quarto_path:

  Path to the Quarto (.qmd) file to create.

- title:

  Title for the presentation.

- use_plotly:

  Logical, whether to convert plots to plotly.

## Value

Invisibly returns TRUE if slides were created.
