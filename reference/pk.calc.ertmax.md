# Calculate the midpoint collection time of the maximum excretion rate

Calculate the midpoint collection time of the maximum excretion rate

## Usage

``` r
pk.calc.ertmax(
  conc,
  volume,
  time,
  duration.conc,
  check = TRUE,
  first.tmax = NULL,
  options = list()
)
```

## Arguments

- conc:

  The concentration in the excreta (e.g., urine or feces)

- volume:

  The volume (or mass) of the sample

- time:

  The starting time of the collection interval

- duration.conc:

  The duration of the collection interval

- check:

  Should the concentration and time data be checked?

- first.tmax:

  If TRUE, return the first time of maximum excretion rate; otherwise,
  return the last

- options:

  List of PKNCA global options set

## Value

The midpoint collection time of the maximum excretion rate, or NA if not
available
