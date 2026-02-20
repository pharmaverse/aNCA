# aNCA (development version)

## Features added

* CDISC ZIP export now includes a `Pre_Specs.xlsx` file with variable-level metadata for each selected dataset (#998)
* Enhancements to the slides outputs including grouping by PKNCA groups, dose profile, and additional grouping variables (#791)
* Option to include and apply NCA flag rules with reasons (NCAwXRS) as defined by ADNCA standards. Any record populated within these columns will be excluded for the NCA (#752)
* R script exported in ZIP folder to re-run and replicate App outputs (#789)
* Individual and Mean plots tabs now created using the same function, so the layout and plot themes are consistent across both plots (#712)
* New flagging rule for lambda-z calculations based on r-squared, R2 (#834)
* New Parameter Selection section in NCA tab allowing to select parameters by study type (#795)
* The App optionally maps end of sample collection (AEFRLT) for excretion rate parameter calculations: ERTLST, ERTMAX. (#745)
* Option to upload multiple input files, which will be bound together to form a single ADNCA data set (#821)
* BLQ imputation rules can be applied to the NCA via `NCA Setup > Data Imputation` (#139)
* Section `General Exclusions` allowing to perform in-App NCA exclusions (#851)
* Mean plots have been added in the TLGs section, with BLQ handling (#555)
* CMAX automatically selected in box plots if available (#890)
* Allow user to select additional `grouping variables` (chosen in the mapping) for ratio calculations (#868)
* General button at top page to save all NCA results, settings & draft slides as a ZIP file (#638)
* Settings are now uploaded on initial opening of the app in the data tab, and applied to the next steps (#860)
* Settings file has been converted from rds to yaml, allowing better readability and editing for users. (#901)
* WTBL and WTBLU columns added to the data mapping, for optional conversion of dose to adjust to body weight for excretion calculations (#959)
* Slope selector table for half life adjustments uses time to choose the point of interest. Also it is aesthetics have been polished (#956)
* Partial interval parameters section now allows other calculations than `AUCINT`, such as `RCAMINT`, `AUCINTD` or `CAVGINT` among others (#524)
* Slope selector plots count with grouping options (#333)
* Add x/y axis limits for the exploration plots (#817) and facet titles including subject count (#894)

## Bugs fixed
* ZIP folder with results will now include the exploration tab outputs: individual plots, mean plots (#794)
* Updated TMAX label from Time of CMAX to Time of CMAX Observation (#787)
* Bug fix for box/violin plots that were crashing when violin option selected (#786)
* Summary statistic table changes in grouping variables won't remove previous summary settings (#840)
* No longer offering direct PK calculations for renal clearance (RENALCL) parameters. PKNCA is currently inaccurate. Instead, the ratios table should be used (#781)
* No longer offering PK calculations for PKNCA multidose parameters; mean residence time (MRTMDO, MRTMDP), steady state volume of distribution (VSSMDP, VSSMDO) and time above (TAT). They are not really able to be calculated using PKNCA (#869)
* Bug fix for settings upload via zip file output (#832)
* Bug fix for plotting section where if there is >1 unit the axis label will contain both unique units. (#818)
* Units table bugs fixed, so it is filtered based on NCA setup and the table is searchable for each column (#870)
* Bug fix to allow for unrecognized units to be used in AVALU and DOSEU (#861)
* Bug fix so NA units are allowed in the data (as per CDISC guidelines for NA samples) and not treated as a unique unit (#907)
* Bug fix so app doesn't crash if NCA is rerun with an error (e.g. No exclusion REASON) (#913)
* NCA results flagging logic updated to include Missing column and correctly
identify difference between missing and not requested (#934)
* Pagination controls in the slope selector and the interactivity of the plots is less buggy (#956)
* Creation of intervals reworked to prevent doses being combined if no samples are taken post dose (#963)

# aNCA 0.1.0

-   Initial CRAN submission.
