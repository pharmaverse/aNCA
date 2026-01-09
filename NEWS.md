# aNCA (development version)

# aNCA 0.2.0

## Features added

* Enhancements to the slides outputs including grouping by PKNCA groups, dose profile, and additional grouping variables (#791)
* Option to include and apply NCA flag rules with reasons (NCAwXRS) as defined by ADNCA standards. Any record populated within these columns will be excluded for the NCA (#752)
* Individual and Mean plots tabs now created using the same function, so the layout and plot themes are consistent across both plots (#712)
* New flagging rule for lambda-z calculations based on r-squared, R2 (#834)
* New Parameter Selection section in NCA tab allowing to select parameters by study type (#795)
* The App optionally maps end of sample collection (AEFRLT) for excretion rate parameter calculations: ERTLST, ERTMAX. (#745)
* Option to upload multiple input files, which will be bound together to form a single ADNCA data set (#821)
* Section `General Exclusions` allowing to perform in-App NCA exclusions (#851)
* Mean plots have been added in the TLGs section, with BLQ handling (#555)


## Bugs fixed
* ZIP folder with results will now include the exploration tab outputs: individual plots, mean plots (#794)
* Updated TMAX label from Time of CMAX to Time of CMAX Observation (#787)
* Bug fix for box/violin plots that were crashing when violin option selected (#786)
* Summary statistic table changes in grouping variables won't remove previous summary settings (#840)
* No longer offering direct PK calculations for renal clearance (RENALCL) parameters. PKNCA is currently inaccurate. Instead, the ratios table should be used (#781)
* Bug fix for settings upload via zip file output (#832)

* Bug fix for plotting section where if there is >1 unit the axis label will contain both unique units. (#818)


# aNCA 0.1.0

-   Initial CRAN submission.
