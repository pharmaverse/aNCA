# Helper Logic to parse and structure settings YAML

Supports both the legacy flat format (top-level `settings` key) and the
versioned format (top-level `current` key). For versioned files the most
recent version is returned by default. Use `version` to select a
specific version by index or comment. The full versioned object is
attached as attribute `"versioned"` so callers can offer version
selection.

## Usage

``` r
read_settings(path, version = 1L)
```

## Arguments

- path:

  Character string with path to the settings YAML file.

- version:

  Version selector for versioned settings files. Either an integer index
  (1 = most recent, 2 = second, etc.) or a character string matched
  against the version `comment` field. Defaults to `1L` (most recent).
  Ignored for non-versioned (legacy) settings files.

## Value

A list with parsed settings. For versioned files, the attribute
`"versioned"` contains the full
[`read_versioned_settings()`](https://pharmaverse.github.io/aNCA/reference/read_versioned_settings.md)
result.
