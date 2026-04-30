# Changelog

## aNCA (development version)

### Features added

- Ratio Calculations UI replaced with formula-style cards showing
  `PPTESTCD = Test [Group] / Ref [Group] × Factor` layout
  ([\#1250](https://github.com/pharmaverse/aNCA/issues/1250))
- Parameter Exclusions: exclude individual PK parameter rows from
  descriptive statistics and ADPP export via PPSUMFL/PPSUMRSN flags
  ([\#1040](https://github.com/pharmaverse/aNCA/issues/1040))
- Partial Interval Calculations table now starts empty by default and
  includes a `(-) Remove Row/s` button matching the Ratio Calculations
  style ([\#1249](https://github.com/pharmaverse/aNCA/issues/1249))
- Settings upload now auto-restores the previous session: auto-applies
  mapping, filters, and data processing, navigates to the saved tab, and
  auto-runs NCA if it was previously run. Incompatible settings degrade
  gracefully with user notifications
  ([\#1225](https://github.com/pharmaverse/aNCA/issues/1225))
- Interval/partial parameters (e.g. AUCINT_0-20) are now selectable in
  the ratio calculations Test/Ref Parameter dropdowns, with correct
  start/end filtering in the ratio computation
  ([\#1135](https://github.com/pharmaverse/aNCA/issues/1135))
- [`run_app()`](https://pharmaverse.github.io/aNCA/reference/run_app.md)
  now accepts a `settings` parameter to pre-load a YAML settings file on
  startup ([\#514](https://github.com/pharmaverse/aNCA/issues/514))
- Exploration sidebars: “View Exports” button opens a scrollable gallery
  modal showing all saved plots inline with name, type, timestamp
  headers and a remove option
  ([\#1137](https://github.com/pharmaverse/aNCA/issues/1137))
- Added “Min. Points for Half-life” setting in NCA \> Settings \>
  General Settings, allowing users to configure PKNCA’s `min.hl.points`
  option (range 2–10, default 3)
  ([\#1155](https://github.com/pharmaverse/aNCA/issues/1155))
- Settings version control: single YAML file stores multiple versions
  with metadata (timestamp, comment, dataset, aNCA version, active tab).
  Save button in header, version selection modal on upload, version
  delete support
  ([\#1103](https://github.com/pharmaverse/aNCA/issues/1103))
- ADPP now includes CRITy/CRITyFL columns for each checked flag rule and
  PPSUMFL/PPSUMRSN columns indicating summary exclusion status. Flagged
  records are excluded from descriptive statistics and parameter plot
  statistics, with an optional toggle to overlay excluded points as
  crosses ([\#1141](https://github.com/pharmaverse/aNCA/issues/1141))
- Right-side sidebars (Exploration, NCA Parameter Plots, TLG) can now be
  resized by dragging the left edge. Default width increased to 250px,
  left nav sidebar reduced to 150px
  ([\#1156](https://github.com/pharmaverse/aNCA/issues/1156))
- Moved `rlistings`, `officer`, and `flextable` from Imports to Suggests
  with user-facing notifications when missing
  ([\#1106](https://github.com/pharmaverse/aNCA/issues/1106))
- Save button is now enabled after data mapping with progressive
  content: exploration plots and settings before NCA, full export after
  NCA ([\#1136](https://github.com/pharmaverse/aNCA/issues/1136))
- General Exclusions: “Excl. TLG” checkbox per exclusion entry sets
  PKSUM1F to “Y”, filtering those rows from TLGs
  ([\#1018](https://github.com/pharmaverse/aNCA/issues/1018))
- NCA Setup filter pickers reordered to Analyte, Specimen, NCA Profile
  with bidirectional cascading updates between Analyte and Specimen
  ([\#1114](https://github.com/pharmaverse/aNCA/issues/1114))
- Column mapping is now included in settings YAML export and restored on
  upload, with validation against available columns
  ([\#1104](https://github.com/pharmaverse/aNCA/issues/1104))
- Ratio calculations table is now included in settings YAML export and
  restored on upload, with validation against available parameters and
  groups ([\#1091](https://github.com/pharmaverse/aNCA/issues/1091))
- Data tab filters are now included in the settings YAML file and
  restored on upload, for both standalone settings download and ZIP
  export ([\#1082](https://github.com/pharmaverse/aNCA/issues/1082))
- Non-standard grouping variables (chosen in the data mapping) are now
  included as columns in ADPP and ADNCA outputs
  ([\#1077](https://github.com/pharmaverse/aNCA/issues/1077))
- Searchable PK parameter reference table added to NCA \> Setup, showing
  metadata, app location, and PKNCA function for each parameter
  ([\#1023](https://github.com/pharmaverse/aNCA/issues/1023))
- Settings YAML units can now contain just `PPTESTCD` and `PPSTRESU`
  (default target units).
  ([\#1027](https://github.com/pharmaverse/aNCA/issues/1027))
- Exploration plots: toggle legend visibility, improved tooltips with
  color-by variable, and correct axis/legend labels
  ([\#988](https://github.com/pharmaverse/aNCA/issues/988))
- Exploration plots: “Add to Exports” button saves named plot snapshots
  to the ZIP export. When custom snapshots exist for a plot type, only
  the snapshots are exported (the default plot is omitted). QC plot also
  included in the export tree
  ([\#1002](https://github.com/pharmaverse/aNCA/issues/1002))
- CDISC ZIP export now includes a `Pre_Specs.xlsx` file with
  variable-level metadata for each selected dataset
  ([\#998](https://github.com/pharmaverse/aNCA/issues/998)) and a
  session information file
  ([\#829](https://github.com/pharmaverse/aNCA/issues/829))
- New “About” tab in the app sidebar with links, citation, authors,
  license, version info, and a “Copy session info” button
  ([\#1015](https://github.com/pharmaverse/aNCA/issues/1015))
- Export filenames use STUDYID as fallback when no project name is set,
  date suffix removed
  ([\#1000](https://github.com/pharmaverse/aNCA/issues/1000))
- Project name auto-populated from STUDYID on data upload
  ([\#1000](https://github.com/pharmaverse/aNCA/issues/1000))
- Enhancements to the slides outputs including grouping by PKNCA groups,
  dose profile, and additional grouping variables
  ([\#791](https://github.com/pharmaverse/aNCA/issues/791))
- Option to include and apply NCA flag rules with reasons (NCAwXRS) as
  defined by ADNCA standards. Any record populated within these columns
  will be excluded for the NCA
  ([\#752](https://github.com/pharmaverse/aNCA/issues/752))
- R script exported in ZIP folder to re-run and replicate App outputs
  ([\#789](https://github.com/pharmaverse/aNCA/issues/789))
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
- BLQ imputation rules can be applied to the NCA via
  `NCA Setup > Data Imputation`
  ([\#139](https://github.com/pharmaverse/aNCA/issues/139))
- Section `General Exclusions` allowing to perform in-App NCA exclusions
  ([\#851](https://github.com/pharmaverse/aNCA/issues/851))
- Mean plots have been added in the TLGs section, with BLQ handling
  ([\#555](https://github.com/pharmaverse/aNCA/issues/555))
- CMAX automatically selected in box plots if available
  ([\#890](https://github.com/pharmaverse/aNCA/issues/890))
- Allow user to select additional `grouping variables` (chosen in the
  mapping) for ratio calculations
  ([\#868](https://github.com/pharmaverse/aNCA/issues/868))
- General button at top page to save all NCA results, settings & draft
  slides as a ZIP file
  ([\#638](https://github.com/pharmaverse/aNCA/issues/638))
- Settings are now uploaded on initial opening of the app in the data
  tab, and applied to the next steps
  ([\#860](https://github.com/pharmaverse/aNCA/issues/860))
- Settings file has been converted from rds to yaml, allowing better
  readability and editing for users.
  ([\#901](https://github.com/pharmaverse/aNCA/issues/901))
- WTBL and WTBLU columns added to the data mapping, for optional
  conversion of dose to adjust to body weight for excretion calculations
  ([\#959](https://github.com/pharmaverse/aNCA/issues/959))
- Slope selector table for half life adjustments uses time to choose the
  point of interest. Also it is aesthetics have been polished
  ([\#956](https://github.com/pharmaverse/aNCA/issues/956))
- Partial interval parameters section now allows other calculations than
  `AUCINT`, such as `RCAMINT`, `AUCINTD` or `CAVGINT` among others
  ([\#524](https://github.com/pharmaverse/aNCA/issues/524))
- Slope selector plots count with grouping options
  ([\#333](https://github.com/pharmaverse/aNCA/issues/333))
- Add x/y axis limits for the exploration plots
  ([\#817](https://github.com/pharmaverse/aNCA/issues/817)) and facet
  titles including subject count
  ([\#894](https://github.com/pharmaverse/aNCA/issues/894))
- Settings upload and processing is flexible, so non-data specific
  template settings can be uploaded
  ([\#993](https://github.com/pharmaverse/aNCA/issues/993))
- Mapping will allow custom numeric input values instead of columns for
  `ADOSEDUR` and `TRTRINT`
  ([\#1051](https://github.com/pharmaverse/aNCA/issues/1051))
- Help buttons have been included/updated for most App sections:
  `Parameter Selection`, `Slope Selector`, `Additional Analysis` and
  `Partial Interval calculations`
  ([\#975](https://github.com/pharmaverse/aNCA/issues/975))
- Removed `methods`, `scales`, and `stringr` from package dependencies,
  replacing all usages with base R equivalents
  ([\#1108](https://github.com/pharmaverse/aNCA/issues/1108))
- SelectInputs updated using a new function to ensure all widgets
  include variable labels.
  ([\#899](https://github.com/pharmaverse/aNCA/issues/899))
- Export modal now allows users to select which slide sections (mean
  plots, statistics, line plots, box plots, individual plots/parameters,
  additional analysis) to include in PPTX and HTML exports; box plot
  parameters are also configurable
  ([\#972](https://github.com/pharmaverse/aNCA/issues/972))
- Settings file now outputs and time duplicate exclusions and processes
  them automatically upon settings upload
  ([\#1195](https://github.com/pharmaverse/aNCA/issues/1195))
- Parameter selection UI replaced with an interactive checkbox matrix
  (study types × parameters). Includes Select all, Defaults, and Clear
  all buttons.

### Bugs fixed

- Last dose interval end time now extends to the last observed sample
  instead of being cut off at TRTRINT (tau), ensuring all collected data
  points are included in NCA calculations
  ([\#1235](https://github.com/pharmaverse/aNCA/issues/1235))
- Fixed NA `PPSTRESU` handling across NCA results: descriptive
  statistics no longer crash when a parameter group has all-NA units,
  and manual interval parameters (e.g., RCAMINT) no longer get `NA`
  appended to their column names
  ([\#1216](https://github.com/pharmaverse/aNCA/issues/1216))
- [`get_settings_code()`](https://pharmaverse.github.io/aNCA/reference/get_settings_code.md)
  now reads mapping, filters, ratio table, and units from the settings
  YAML instead of using hardcoded defaults. Legacy YAML files without
  these fields still work via fallback. The `mapping` parameter has been
  removed ([\#1189](https://github.com/pharmaverse/aNCA/issues/1189))
- SASS compilation moved from runtime (`app.R`) to a
  `data-raw/compile_css.R` script, fixing startup crashes on read-only
  deployments
  ([\#1107](https://github.com/pharmaverse/aNCA/issues/1107))
- ZIP folder with results will now include the exploration tab outputs:
  individual plots, mean plots
  ([\#794](https://github.com/pharmaverse/aNCA/issues/794))
- Updated TMAX label from Time of CMAX to Time of CMAX Observation
  ([\#787](https://github.com/pharmaverse/aNCA/issues/787))
- Bug fix for box/violin plots that were crashing when violin option
  selected ([\#786](https://github.com/pharmaverse/aNCA/issues/786))
- Summary statistic table changes in grouping variables won’t remove
  previous summary settings
  ([\#840](https://github.com/pharmaverse/aNCA/issues/840))
- No longer offering direct PK calculations for renal clearance
  (RENALCL) parameters. PKNCA is currently inaccurate. Instead, the
  ratios table should be used
  ([\#781](https://github.com/pharmaverse/aNCA/issues/781))
- No longer offering PK calculations for PKNCA multidose parameters;
  mean residence time (MRTMDO, MRTMDP), steady state volume of
  distribution (VSSMDP, VSSMDO) and time above (TAT). They are not
  really able to be calculated using PKNCA
  ([\#869](https://github.com/pharmaverse/aNCA/issues/869))
- Bug fix for settings upload via zip file output
  ([\#832](https://github.com/pharmaverse/aNCA/issues/832))
- Bug fix for plotting section where if there is \>1 unit the axis label
  will contain both unique units.
  ([\#818](https://github.com/pharmaverse/aNCA/issues/818))
- Units table bugs fixed, so it is filtered based on NCA setup and the
  table is searchable for each column
  ([\#870](https://github.com/pharmaverse/aNCA/issues/870))
- Bug fix to allow for unrecognized units to be used in AVALU and DOSEU
  ([\#861](https://github.com/pharmaverse/aNCA/issues/861))
- Bug fix so NA units are allowed in the data (as per CDISC guidelines
  for NA samples) and not treated as a unique unit
  ([\#907](https://github.com/pharmaverse/aNCA/issues/907))
- Bug fix so app doesn’t crash if NCA is rerun with an error (e.g. No
  exclusion REASON)
  ([\#913](https://github.com/pharmaverse/aNCA/issues/913))
- NCA results flagging logic updated to include Missing column and
  correctly identify difference between missing and not requested
  ([\#934](https://github.com/pharmaverse/aNCA/issues/934))
- Pagination controls in the slope selector and the interactivity of the
  plots is less buggy
  ([\#956](https://github.com/pharmaverse/aNCA/issues/956))
- Creation of intervals reworked to prevent doses being combined if no
  samples are taken post dose
  ([\#963](https://github.com/pharmaverse/aNCA/issues/963))
- Parameter selection no longer resets after changes to NCA setup and
  slope selector- apart from changes to analyte and pcspec that change
  the study types detected
  ([\#1008](https://github.com/pharmaverse/aNCA/issues/1008))
- Prevent a crash when selecting already defined identity variables
  (i.e, `DOSETRT`) for the `Additional Grouping Variables` in the
  `Mapping Tab`
  ([\#1060](https://github.com/pharmaverse/aNCA/issues/1060))
- Filtering will now correctly also affect all the input widgets in NCA
  setup ([\#1092](https://github.com/pharmaverse/aNCA/issues/1092))
- All Results input widgets now shows each manual interval parameter
  with its range suffix (e.g. `AUCINT_0-12`, `CAVGINT_0-24`) instead of
  collapsing them into a single entry
  ([\#1146](https://github.com/pharmaverse/aNCA/issues/1146))
- Boxplot parameter selector and excretion end time column selector now
  restrict to single selection, preventing errors when downstream code
  expects a single value. Boxplot selector also shows each manual
  interval parameter with its range suffix (e.g. `AUCINT_0-12`,
  `CAVGINT_0-24`) instead of collapsing them into a single entry
  ([\#1148](https://github.com/pharmaverse/aNCA/issues/1148))
- Manual interval parameter renaming now uses dose-relative times
  (`start_dose`/`end_dose`) consistently across descriptive statistics,
  parameter plots, and boxplots, matching `pivot_wider_pknca_results`
  ([\#1169](https://github.com/pharmaverse/aNCA/issues/1169))
- Descriptive statistics parameter columns are now correctly displayed
  when using the `selector_label` widget, and duplicate rows from the
  concentration data join are deduplicated
  ([\#1169](https://github.com/pharmaverse/aNCA/issues/1169))
- Fixed `PKNCA_update_data_object` custom units table join using
  hardcoded `by = c("PPTESTCD", "PPORRESU")` instead of dynamic keys,
  which failed when the units table included group columns like `PARAM`
  or `PCSPEC`
  ([\#1159](https://github.com/pharmaverse/aNCA/issues/1159))
- Fixed `DOSNOA` computation using specimen-level grouping (including
  PCSPEC/PARAM), causing urine-only Day 10 data to get `DOSNOA=1`
  instead of `DOSNOA=2`, leading to incorrect dose time matching
  ([\#1116](https://github.com/pharmaverse/aNCA/issues/1116))
- Fixed “Summarise by” selector in Matrix Ratios having no effect due to
  input ID mismatch (`summarygroups` vs `summary_groups`)
  ([\#1198](https://github.com/pharmaverse/aNCA/issues/1198))

## aNCA 0.1.0

CRAN release: 2025-12-09

- Initial CRAN submission.
