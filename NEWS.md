# aNCA (development version)

## Testing

* Add 100% line coverage for `g_pkcg.R`, `g_lineplot.R`, `l_pkcl01.R`, and TLG Shiny modules (#1351)

## Features

### Settings & Configuration
* Settings upload auto-restores the full session: mapping, filters, data processing, tab navigation, and auto-runs NCA if previously run. Incompatible settings degrade gracefully with notifications (#1225)
* Settings version control: YAML file stores multiple versions with metadata. Save button in header, version selection on upload, version delete support (#1103)
* Settings file converted from RDS to YAML for readability and manual editing (#901)
* Settings are uploaded on initial opening of the app in the data tab and applied to the next steps (#860)
* Column mapping, data filters, ratio table, units, and time-duplicate exclusions are now included in settings YAML export/import (#1082, #1091, #1104, #1195)
* `run_app()` accepts a `settings` parameter to pre-load a YAML settings file on startup (#514)
* Settings upload is flexible — non-data-specific template settings can be uploaded (#993)

### Exploration
* "Copy Plot Code" button in the right sidebar opens a modal with a self-contained R script for the current plot, including data loading, mapping, filtering, and PNG/HTML export (#1327)

### NCA Setup
* Renamed "Aggregate Subject" label to "Mean across subjects" in ratio calculations for clarity; updated help text to explain matching mechanics (#1297)
* Parameter Selection tab now contains Partial Intervals, Ratio Calculations, and Units alongside the parameter matrix. The former Settings tab is renamed to General Settings (#1239)
* Parameter selection UI replaced with an interactive checkbox matrix (study types × parameters) with Select All, Defaults, and Clear All buttons (#795)
* Partial interval parameters section supports calculations beyond `AUCINT`: `RCAMINT`, `AUCINTD`, `CAVGINT`, and others. Table starts empty by default with a Remove Row button (#524, #1249)
* "Min. Points for Half-life" setting added (range 2–10, default 3) (#1155)
* BLQ imputation rules via `NCA Setup > Data Imputation` (#139)
* General Exclusions section for in-app NCA exclusions, with "Excl. TLG" checkbox per entry (#851, #1018)
* Parameter Exclusions tab: exclude individual PK parameter rows from descriptive statistics and ADPP export via PPSUMFL/PPSUMRSN flags (#1040)
* NCA flag rules (NCAwXRS) from ADNCA standards — flagged records are excluded from NCA (#752)
* New flagging rule for lambda-z based on R² (#834)
* Filter pickers reordered to Analyte → Specimen → NCA Profile with bidirectional cascading (#1114)
* Optional settings (`slope_rules`, `int_parameters`, `ratio_table`) normalized to `NULL` when empty (#1262)

### Ratio Calculations
* Bioavailability removed as a dedicated pipeline — FABS/FREL now computed exclusively via the ratio table (#1260)
* Ratio Calculations UI replaced with formula-style fraction cards (#1250)
* Interval/partial parameters (e.g. AUCINT_0-20) selectable in ratio Test/Ref Parameter dropdowns (#1135)
* Additional grouping variables available for ratio calculations (#868)
* Webpage documentation available in help button for further explanations on how to do ratio calculations (#1300)

### Exploration & Plots
* Dose-normalised exploration plots: toggle between default, dose-normalised, or both overlaid on individual and mean plots (#986)
* Exploration sidebars: "Add to Exports" saves named plot snapshots to ZIP; "View Exports" shows a gallery modal (#1002, #1137)
* Toggle legend visibility, improved tooltips, correct axis/legend labels (#988)
* X/Y axis limits for exploration plots, facet titles with subject count (#817, #894, #984)
* Individual and Mean plots use the same function for consistent layout and themes (#712)
* Right-side sidebars resizable by dragging; default width 250px (#1156)

### Export & Output
* PowerPoint export includes a PPTESTCD glossary slide after the title slide, listing all PK parameter codes and their full names (#1326)
* General button at top of page to save all NCA results, settings, and draft slides as a ZIP file (#638)
* Dose-normalised summary slides added to PPT/QMD export, controlled via Customise Slides modal (#1054)
* Export modal allows selecting which slide sections to include in PPTX/HTML exports (#972)
* CDISC ZIP includes `Pre_Specs.xlsx` with variable-level metadata and session info (#998, #829)
* ADPP includes CRITy/CRITyFL columns for flag rules and PPSUMFL/PPSUMRSN for summary exclusion status (#1141)
* Non-standard grouping variables included in ADPP and ADNCA outputs (#1077)
* R script exported in ZIP to replicate app outputs (#789)
* Save button enabled after data mapping with progressive content (#1136)
* Export filenames use STUDYID as fallback; project name auto-populated from STUDYID (#1000)
* Slide outputs grouped by PKNCA groups, dose profile, and additional grouping variables (#791)
* Mean plots added to TLGs section with BLQ handling (#555)
* CMAX auto-selected in box plots if available (#890)

### Data & Mapping
* ADNCA now includes `PKSUM1RS` column storing the general exclusion reason when `PKSUM1F = "Y"` (#1331)
* Upload multiple input files, bound into a single ADNCA dataset (#821)
* Optional mapping of AEFRLT for excretion rate parameters (ERTLST, ERTMAX) (#745)
* WTBL/WTBLU columns for dose-to-body-weight conversion in excretion calculations (#959)
* Custom numeric input values for ADOSEDUR and TRTRINT instead of column mapping (#1051)
* SelectInputs updated to include variable labels (#899)

### Documentation & UI
* UI consistency pass: standardized dropdown labels to "Select the...", help button placement to right-aligned, "colour" to "color", renamed Save to "Export as ZIP", simplified download button text, and added "Short Parameter"/"Specimen" columns to Units table (#1333)
* Searchable PK parameter reference table in NCA > Setup (#1023)
* R Script Walkthrough vignette added to pkgdown website (#1090)
* Ratio Calculations vignette documenting all ratio types (#1251)
* "About" tab with links, citation, authors, license, version, and "Copy session info" button (#1015)
* Help buttons added/updated for Parameter Selection, Slope Selector, Additional Analysis, and Partial Interval Calculations (#975)
* Slope selector table uses time-based selection with improved aesthetics and grouping options (#956, #333)

## Bug fixes

### CDISC Export
* `CRITxFL` now uses `""` instead of `"N"` when criterion is violated. `CRITx` shows the acceptance criterion with inverted operator (e.g. flag rule `R2ADJ < 0.7` produces `CRITx = "R2ADJ >= 0.7"`). `CRITxFL = "Y"` means criterion satisfied (#1332)

### NCA Calculations
* Renal clearance (RENALCL) removed from direct PK calculations (inaccurate in PKNCA) — use ratio table instead (#781)
* Multidose parameters (MRTMDO, MRTMDP, VSSMDO, VSSMDP, TAT) removed from direct calculations (#869)
* Last dose interval end time extends to last observed sample instead of being cut off at tau (#1235)
* Interval creation reworked to prevent doses being combined when no post-dose samples exist (#963)
* DOSNOA computation fixed for specimen-level grouping — urine-only data no longer gets incorrect dose numbering (#1116)
* Dose-aware AUCint parameters now share the same PPTESTCD as their non-dose-aware counterparts in CDISC exports, with `PPANMETH` indicating the analytical method. Internal PPTESTCDs renamed from misleading `D` suffix (e.g. `AUCINTD`) to lowercase `da` suffix (e.g. `AUCINTda`). Fixed wrong PPTEST label for `AUCINTD` which said "Normalized by Dose" (#1242)
* Optional settings (`slope_rules`, `int_parameters`, `ratio_table`) are now normalized to `NULL` when empty, instead of persisting as 0-row data frames throughout the app and settings pipeline (#1262)
* Interval-specific parameters (`aucint.*`, `cav.int.*`) excluded from the Parameter Selection matrix — they require finite sub-intervals and must be configured via Partial Interval Calculations (#1309)

### Ratio Calculations
* Fixed `Aggregate Subject = yes/if-needed` not aggregating reference values, and ratio columns not appearing in results (#1273)

### NCA Results & Export
* Interval parameters (e.g. `AUCINT_0-24`) now display human-readable labels in parameter selectors and boxplot y-axis, instead of raw PPTESTCDs (#1305)
* Descriptive statistics were silently ungrouped when exported before visiting the tab — now falls back to default grouping columns (#1264)
* Fixed NA `PPSTRESU` handling: descriptive statistics no longer crash on all-NA unit groups, and manual interval parameters no longer get `NA` in column names (#1216)
* `get_settings_code()` reads mapping, filters, ratio table, and units from YAML instead of hardcoded defaults (#1189)
* All Results widgets show interval parameters with range suffix (e.g. `AUCINT_0-12`) instead of collapsing into one entry (#1146)
* Interval parameter renaming uses dose-relative times consistently across statistics, plots, and boxplots (#1169)
* Descriptive statistics columns display correctly with `selector_label` widget; duplicate rows deduplicated (#1169)
* NCA results flagging correctly distinguishes missing vs not-requested parameters (#934)
* Custom units table join uses correct keys instead of hardcoded columns (#1159)
* "Summarise by" selector in Matrix Ratios fixed (input ID mismatch) (#1198)

### Settings & Upload
* SASS compilation moved from runtime to build-time script, fixing startup crashes on read-only deployments (#1107)
* Settings upload via ZIP file fixed (#832)
* App no longer crashes if NCA is rerun with an error (#913)
* Parameter selection no longer resets after NCA setup changes (except analyte/specimen changes) (#1008)
* Filtering correctly affects all NCA setup input widgets (#1092)
* Selecting already-defined identity variables for Additional Grouping Variables no longer crashes (#1060)
* Summary statistic grouping variable changes no longer remove previous settings (#840)

### Exploration & Plots
* ZIP export includes exploration tab outputs: individual and mean plots (#794)
* Box/violin plots no longer crash when violin option selected (#786)
* Boxplot parameter selector restricted to single selection; shows interval parameters with range suffix (#1148)
* Axis labels show both unique units when >1 unit exists (#818)

### Units & Data
* Units table filtered based on NCA setup; searchable per column (#870)
* Unrecognized units allowed in AVALU and DOSEU (#861)
* NA units allowed in data per CDISC guidelines (#907)
* TMAX label corrected from "Time of CMAX" to "Time of CMAX Observation" (#787)
* Pagination controls in the slope selector are less buggy (#956)

### TLG
* Fixed one-sided whiskers (Upper/Lower) collapsing to zero height in mean plots (`pkcg03`) (#1316)

## Dependency changes

* Removed `tern` and `nestcolor` — TLG plots (`pkcg01`, `pkcg02`, `pkcg03`) now use inline ggplot2 code (#1316)
* `rlistings`, `officer`, and `flextable` moved from Imports to Suggests — the app notifies users when these are missing (#1106)
* Removed `methods`, `scales`, and `stringr` from package dependencies, replaced with base R equivalents (#1108)
* Settings YAML units accept minimal format: just `PPTESTCD` and `PPSTRESU` (#1027)

# aNCA 0.1.0

* Initial CRAN submission.
