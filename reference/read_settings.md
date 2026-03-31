# Helper Logic to parse and structure settings YAML

Supports both the legacy flat format (top-level `settings` key) and the
versioned format (top-level `current` key). For versioned files the most
recent version is returned by default. The full versioned object is
attached as attribute `"versioned"` so callers can offer version
selection.

## Usage

``` r
read_settings(path)
```

## Arguments

- path:

  Character string with path to the settings YAML file.

## Value

A list with parsed settings. For versioned files, the attribute
`"versioned"` contains the full
[`read_versioned_settings()`](https://pharmaverse.github.io/aNCA/reference/read_versioned_settings.md)
result.
