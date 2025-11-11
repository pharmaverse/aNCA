# Render dose escalation results to HTML via Quarto

Used internally to create and render a .qmd file to HTML.

## Usage

``` r
create_html_dose_slides(res_dose_slides, path, title)
```

## Arguments

- res_dose_slides:

  List of results for each dose group.

- path:

  Path to the output HTML file.

- title:

  Title for the presentation.

## Value

Invisibly returns TRUE if rendering succeeded.
