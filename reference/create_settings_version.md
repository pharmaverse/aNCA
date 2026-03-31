# Create a versioned settings entry

Wraps a settings payload with metadata (timestamp, comment, dataset
name, aNCA version, active tab) for storage in a versioned settings YAML
file.

## Usage

``` r
create_settings_version(settings_data, comment = "", dataset = "", tab = "")
```

## Arguments

- settings_data:

  List with the settings payload (mapping, parameters, NCA setup,
  filters, slope_rules, etc.).

- comment:

  Optional character string with a user comment.

- dataset:

  Optional character string identifying the dataset used.

- tab:

  Optional character string with the active app tab.

## Value

A named list representing one version entry.
