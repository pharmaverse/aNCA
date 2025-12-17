# aNCA (development version)

# aNCA 0.2.0

## Features added

* Rename all "ADPC" label mentions in the App to "ADNCA", which is the CDISC standard the App promotes (#778)
* Interface includes now a color legend next to the pivoted NCA results to indicate missing and flagged parameters (#779)
* Enhancements to the slides outputs including grouping by PKNCA groups, dose profile, and additional grouping variables (#791)
* Option to include and apply NCA flag rules with reasons (NCAwXRS) as defined by ADNCA standards. Any record populated within these columns will be excluded for the NCA (#752)
* Individual and Mean plots tabs now created using the same function, so the layout and plot themes are consistent across both plots (#712)
* New flagging rule for lambda-z calculations based on r-squared, R2 (#834)
* New Parameter Selection section in NCA tab allowing to select parameters by study type (#795)

## Bugs fixed
* Bug fix for box/violin plots that were crashing when PPSTRES is NA (#785)

* Exact duplicate records (DTYPE = "COPY") and time duplicates (DTYPE = "TIME DUPLICATE") are not removed from the data, but just excluded from the analysis (#765)

* ZIP folder with results will include now the exploration tab outputs: individual plots, mean plots (#794)

* Updated TMAX label from Time of CMAX to Time of CMAX Observation (#787)

* Bug fix for box/violin plots that were crashing when violin option selected (#786)


# aNCA 0.1.0

* Initial CRAN submission.
