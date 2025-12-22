# Changelog

## aNCA (development version)

## aNCA 0.2.0

### Features added

- Enhancements to the slides outputs including grouping by PKNCA groups,
  dose profile, and additional grouping variables
  ([\#791](https://github.com/pharmaverse/aNCA/issues/791))
- Option to include and apply NCA flag rules with reasons (NCAwXRS) as
  defined by ADNCA standards. Any record populated within these columns
  will be excluded for the NCA
  ([\#752](https://github.com/pharmaverse/aNCA/issues/752))
- Individual and Mean plots tabs now created using the same function, so
  the layout and plot themes are consistent across both plots
  ([\#712](https://github.com/pharmaverse/aNCA/issues/712))
- New flagging rule for lambda-z calculations based on r-squared, R2
  ([\#834](https://github.com/pharmaverse/aNCA/issues/834))
- New Parameter Selection section in NCA tab allowing to select
  parameters by study type
  ([\#795](https://github.com/pharmaverse/aNCA/issues/795))
- The App optionally maps end of sample collection (AEFRLT) for
  excretion rate parameter calculations: ERTLST, ERTMAX.
  ([\#745](https://github.com/pharmaverse/aNCA/issues/745))
- Option to upload multiple input files, which will be bound together to
  form a single ADNCA data set
  ([\#821](https://github.com/pharmaverse/aNCA/issues/821))

### Bugs fixed

- ZIP folder with results will now include the exploration tab outputs:
  individual plots, mean plots
  ([\#794](https://github.com/pharmaverse/aNCA/issues/794))

- Updated TMAX label from Time of CMAX to Time of CMAX Observation
  ([\#787](https://github.com/pharmaverse/aNCA/issues/787))

- Bug fix for box/violin plots that were crashing when violin option
  selected ([\#786](https://github.com/pharmaverse/aNCA/issues/786))

- No longer offering direct PK calculations for renal clearance
  (RENALCL) parameters. PKNCA is currently inaccurate. Instead, the
  ratios table should be used
  ([\#781](https://github.com/pharmaverse/aNCA/issues/781))

- Bug fix for settings upload via zip file output
  ([\#832](https://github.com/pharmaverse/aNCA/issues/832))

- Bug fix for plotting section where if there is \>1 unit the axis label
  will contain both unique units.
  ([\#818](https://github.com/pharmaverse/aNCA/issues/818))

## aNCA 0.1.0

CRAN release: 2025-12-09

- Initial CRAN submission.
