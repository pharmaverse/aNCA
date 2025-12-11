# aNCA (development version)

# aNCA 0.2.0

## Features added

* Rename all "ADPC" label mentions in the App to "ADNCA", which is the CDISC standard the App promotes (#778)
* Interface includes now a color legend next to the pivoted NCA results to indicate missing and flagged parameters (#779)
* Option to include and apply NCA flag rules with reasons (NCAwXRS) as defined by ADNCA standards. Any record populated within these columns will be excluded for the NCA (#752)

## Bugs fixed
* Bug fix for box/violin plots that were crashing when PPSTRES is NA (#785)

* Exact duplicate records (DTYPE = "COPY") and time duplicates (DTYPE = "TIME DUPLICATE") are not removed from the data, but just excluded from the analysis (#765) 

* Updated TMAX label from Time of CMAX to Time of CMAX Observation (#787)


# aNCA 0.1.0

* Initial CRAN submission.
