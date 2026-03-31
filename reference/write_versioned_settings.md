# Write a versioned settings YAML file

Writes a list of version entries to a YAML file in the versioned format.
The first entry becomes `current`, the rest go under `previous`.

## Usage

``` r
write_versioned_settings(versions, path)
```

## Arguments

- versions:

  A list of version entries (as returned by
  [`create_settings_version()`](https://pharmaverse.github.io/aNCA/reference/create_settings_version.md)).

- path:

  Character string with the output file path.

## Value

Invisibly returns `path`.
