# Changelog

## aNCA (development version)

### Data & settings

- **Data upload:** Combine multiple files, see accepted formats and size
  limits, and receive clearer upload and mapping errors
  ([\#821](https://github.com/pharmaverse/aNCA/issues/821),
  [\#1365](https://github.com/pharmaverse/aNCA/issues/1365),
  [\#1367](https://github.com/pharmaverse/aNCA/issues/1367)).
- **Data mapping:** Map excretion times and body-weight units, enter
  dose duration and treatment intervals directly, and continue after
  mapping errors or duplicate-row cancellation without stuck loading
  indicators ([\#745](https://github.com/pharmaverse/aNCA/issues/745),
  [\#959](https://github.com/pharmaverse/aNCA/issues/959),
  [\#1051](https://github.com/pharmaverse/aNCA/issues/1051),
  [\#1402](https://github.com/pharmaverse/aNCA/issues/1402),
  [\#1420](https://github.com/pharmaverse/aNCA/issues/1420)).
- **Saved sessions:** Save versioned YAML settings and restore mappings,
  filters, exclusions, ratios, units, and previous NCA runs from YAML or
  ZIP, including reusable templates and startup settings
  ([\#514](https://github.com/pharmaverse/aNCA/issues/514),
  [\#832](https://github.com/pharmaverse/aNCA/issues/832),
  [\#860](https://github.com/pharmaverse/aNCA/issues/860),
  [\#901](https://github.com/pharmaverse/aNCA/issues/901),
  [\#993](https://github.com/pharmaverse/aNCA/issues/993),
  [\#1082](https://github.com/pharmaverse/aNCA/issues/1082),
  [\#1091](https://github.com/pharmaverse/aNCA/issues/1091),
  [\#1103](https://github.com/pharmaverse/aNCA/issues/1103),
  [\#1104](https://github.com/pharmaverse/aNCA/issues/1104),
  [\#1195](https://github.com/pharmaverse/aNCA/issues/1195),
  [\#1225](https://github.com/pharmaverse/aNCA/issues/1225)).
- **Settings stability:** Preserve parameter and summary selections
  during setup changes, apply filters consistently, and ignore invalid
  partial intervals in uploaded settings
  ([\#840](https://github.com/pharmaverse/aNCA/issues/840),
  [\#1008](https://github.com/pharmaverse/aNCA/issues/1008),
  [\#1060](https://github.com/pharmaverse/aNCA/issues/1060),
  [\#1092](https://github.com/pharmaverse/aNCA/issues/1092),
  [\#1347](https://github.com/pharmaverse/aNCA/issues/1347)).

### NCA calculations & results

- **Parameter selection:** Select parameters by study type in a checkbox
  matrix, including metabolite profiles, with intervals, ratios, and
  units together and clearer messages for missing selections
  ([\#795](https://github.com/pharmaverse/aNCA/issues/795),
  [\#1114](https://github.com/pharmaverse/aNCA/issues/1114),
  [\#1239](https://github.com/pharmaverse/aNCA/issues/1239),
  [\#1471](https://github.com/pharmaverse/aNCA/issues/1471),
  [\#1527](https://github.com/pharmaverse/aNCA/issues/1527)).
- **Partial intervals:** Calculate additional interval parameters
  through Partial Interval Calculations and distinguish dose-relative
  ranges consistently in results, statistics, and plot selectors
  ([\#524](https://github.com/pharmaverse/aNCA/issues/524),
  [\#1146](https://github.com/pharmaverse/aNCA/issues/1146),
  [\#1148](https://github.com/pharmaverse/aNCA/issues/1148),
  [\#1169](https://github.com/pharmaverse/aNCA/issues/1169),
  [\#1249](https://github.com/pharmaverse/aNCA/issues/1249),
  [\#1305](https://github.com/pharmaverse/aNCA/issues/1305),
  [\#1309](https://github.com/pharmaverse/aNCA/issues/1309)).
- **Dose intervals:** Corrected urine dose numbering, prevented doses
  without post-dose samples from being combined, and extended the last
  interval to the last observed sample
  ([\#963](https://github.com/pharmaverse/aNCA/issues/963),
  [\#1116](https://github.com/pharmaverse/aNCA/issues/1116),
  [\#1235](https://github.com/pharmaverse/aNCA/issues/1235)).
- **Imputation:** Configure BLQ rules and run NCA without start
  imputation, using the first sample at or after the dose as the
  interval start
  ([\#139](https://github.com/pharmaverse/aNCA/issues/139),
  [\#1121](https://github.com/pharmaverse/aNCA/issues/1121),
  [\#1266](https://github.com/pharmaverse/aNCA/issues/1266)).
- **Half-life selection:** Set minimum point counts, apply R²-based
  flags, and select slope points by time with corrected grouping,
  pagination, and plot rendering
  ([\#333](https://github.com/pharmaverse/aNCA/issues/333),
  [\#834](https://github.com/pharmaverse/aNCA/issues/834),
  [\#956](https://github.com/pharmaverse/aNCA/issues/956),
  [\#1155](https://github.com/pharmaverse/aNCA/issues/1155),
  [\#1378](https://github.com/pharmaverse/aNCA/issues/1378)).
- **Calculation exclusions:** Apply general exclusions and ADNCA flag
  rules, with separate controls for calculation and summary exclusions
  ([\#752](https://github.com/pharmaverse/aNCA/issues/752),
  [\#851](https://github.com/pharmaverse/aNCA/issues/851),
  [\#1018](https://github.com/pharmaverse/aNCA/issues/1018),
  [\#1040](https://github.com/pharmaverse/aNCA/issues/1040),
  [\#1436](https://github.com/pharmaverse/aNCA/issues/1436)).
- **Summary exclusions:** Apply concentration and parameter exclusions
  to their respective summaries while retaining individual records and
  marking excluded concentrations in plots and exports
  ([\#1331](https://github.com/pharmaverse/aNCA/issues/1331),
  [\#1343](https://github.com/pharmaverse/aNCA/issues/1343),
  [\#1356](https://github.com/pharmaverse/aNCA/issues/1356),
  [\#1439](https://github.com/pharmaverse/aNCA/issues/1439),
  [\#1536](https://github.com/pharmaverse/aNCA/issues/1536)).
- **Results accuracy:** Corrected missing-versus-unrequested flags,
  duplicate result rows, and descriptive-statistics grouping when
  exporting before opening the results tab
  ([\#934](https://github.com/pharmaverse/aNCA/issues/934),
  [\#1169](https://github.com/pharmaverse/aNCA/issues/1169),
  [\#1264](https://github.com/pharmaverse/aNCA/issues/1264),
  [\#1479](https://github.com/pharmaverse/aNCA/issues/1479)).
- **Unavailable parameters:** Removed direct RENALCL calculation in
  favor of ratios and removed MRTMDO, MRTMDP, VSSMDO, VSSMDP, and TAT
  from direct calculations
  ([\#781](https://github.com/pharmaverse/aNCA/issues/781),
  [\#869](https://github.com/pharmaverse/aNCA/issues/869)).

### Ratios & units

- **Ratios:** Use formula-style cards for bioavailability and interval
  ratios, with additional grouping variables, corrected reference
  averaging, and working result and summary selectors
  ([\#868](https://github.com/pharmaverse/aNCA/issues/868),
  [\#1135](https://github.com/pharmaverse/aNCA/issues/1135),
  [\#1198](https://github.com/pharmaverse/aNCA/issues/1198),
  [\#1250](https://github.com/pharmaverse/aNCA/issues/1250),
  [\#1260](https://github.com/pharmaverse/aNCA/issues/1260),
  [\#1273](https://github.com/pharmaverse/aNCA/issues/1273),
  [\#1297](https://github.com/pharmaverse/aNCA/issues/1297)).
- **Units:** Search units by parameter, accept missing or unrecognized
  units, and preserve custom and simplified units across settings,
  results, and generated scripts
  ([\#861](https://github.com/pharmaverse/aNCA/issues/861),
  [\#870](https://github.com/pharmaverse/aNCA/issues/870),
  [\#907](https://github.com/pharmaverse/aNCA/issues/907),
  [\#1027](https://github.com/pharmaverse/aNCA/issues/1027),
  [\#1159](https://github.com/pharmaverse/aNCA/issues/1159),
  [\#1190](https://github.com/pharmaverse/aNCA/issues/1190),
  [\#1216](https://github.com/pharmaverse/aNCA/issues/1216),
  [\#1531](https://github.com/pharmaverse/aNCA/issues/1531)).

### Exploration & plots

- **Exploration:** Compare original and dose-normalized profiles,
  customize axes and legends, and use shared facet scales with automatic
  conversion of compatible units
  ([\#712](https://github.com/pharmaverse/aNCA/issues/712),
  [\#817](https://github.com/pharmaverse/aNCA/issues/817),
  [\#818](https://github.com/pharmaverse/aNCA/issues/818),
  [\#894](https://github.com/pharmaverse/aNCA/issues/894),
  [\#984](https://github.com/pharmaverse/aNCA/issues/984),
  [\#986](https://github.com/pharmaverse/aNCA/issues/986),
  [\#988](https://github.com/pharmaverse/aNCA/issues/988),
  [\#1424](https://github.com/pharmaverse/aNCA/issues/1424)).
- **Reusable plots:** Save named plot snapshots to ZIP, browse saved
  plots, and copy R code that reproduces the current exploration plot
  ([\#794](https://github.com/pharmaverse/aNCA/issues/794),
  [\#1002](https://github.com/pharmaverse/aNCA/issues/1002),
  [\#1137](https://github.com/pharmaverse/aNCA/issues/1137),
  [\#1327](https://github.com/pharmaverse/aNCA/issues/1327)).
- **Plot display:** Fixed overlapping axis labels, ignored ticks and
  zero limits, blank panels, incorrect grouping labels, missing titles,
  and box/violin rendering
  ([\#786](https://github.com/pharmaverse/aNCA/issues/786),
  [\#890](https://github.com/pharmaverse/aNCA/issues/890),
  [\#1336](https://github.com/pharmaverse/aNCA/issues/1336),
  [\#1356](https://github.com/pharmaverse/aNCA/issues/1356),
  [\#1441](https://github.com/pharmaverse/aNCA/issues/1441)).
- **Mean profiles:** Corrected subject-based BLQ filtering and one-sided
  whiskers, and restored combined concentration plots that overlay
  subjects by default
  ([\#555](https://github.com/pharmaverse/aNCA/issues/555),
  [\#1316](https://github.com/pharmaverse/aNCA/issues/1316),
  [\#1356](https://github.com/pharmaverse/aNCA/issues/1356)).

### Tables, listings & graphs

- **TLG catalog:** Added concentration and PK summaries, dose-normalized
  and urine outputs, geometric mean ratios with 90% confidence
  intervals, boxplots, dose-proportionality plots, and individual
  listings ([\#1343](https://github.com/pharmaverse/aNCA/issues/1343)).
- **TLG selection:** Search and select outputs by dataset, manage the
  order with bulk controls, and receive explanations for unavailable
  outputs or missing NCA results
  ([\#1335](https://github.com/pharmaverse/aNCA/issues/1335),
  [\#1356](https://github.com/pharmaverse/aNCA/issues/1356),
  [\#1530](https://github.com/pharmaverse/aNCA/issues/1530)).
- **Table customization:** Choose grouping, covariate comparisons,
  statistics, parameters, timepoints, and listing columns, with readable
  group headers and warnings for unavailable grouping variables
  ([\#1343](https://github.com/pharmaverse/aNCA/issues/1343),
  [\#1356](https://github.com/pharmaverse/aNCA/issues/1356),
  [\#1531](https://github.com/pharmaverse/aNCA/issues/1531)).
- **Urine outputs:** Restrict urine tables and listings to urine
  specimens and report missing specimen information
  ([\#1335](https://github.com/pharmaverse/aNCA/issues/1335),
  [\#1343](https://github.com/pharmaverse/aNCA/issues/1343),
  [\#1356](https://github.com/pharmaverse/aNCA/issues/1356),
  [\#1531](https://github.com/pharmaverse/aNCA/issues/1531)).

### Exports & CDISC

- **ZIP exports:** Export results, settings, plots, and TLGs using
  current selections, with PDF/HTML/PNG graphs, XLSX/CSV/PDF tables,
  study-based filenames, and a manifest of output statuses
  ([\#638](https://github.com/pharmaverse/aNCA/issues/638),
  [\#1000](https://github.com/pharmaverse/aNCA/issues/1000),
  [\#1136](https://github.com/pharmaverse/aNCA/issues/1136),
  [\#1344](https://github.com/pharmaverse/aNCA/issues/1344),
  [\#1428](https://github.com/pharmaverse/aNCA/issues/1428)).
- **Slides:** Customize slide sections and grouping, include
  dose-normalized summaries, and add a PK parameter glossary
  ([\#791](https://github.com/pharmaverse/aNCA/issues/791),
  [\#972](https://github.com/pharmaverse/aNCA/issues/972),
  [\#1054](https://github.com/pharmaverse/aNCA/issues/1054),
  [\#1326](https://github.com/pharmaverse/aNCA/issues/1326)).
- **Reproducible scripts:** Export R scripts that use saved mappings,
  filters, ratios, units, and BLQ rules to reproduce app results
  ([\#789](https://github.com/pharmaverse/aNCA/issues/789),
  [\#1189](https://github.com/pharmaverse/aNCA/issues/1189),
  [\#1445](https://github.com/pharmaverse/aNCA/issues/1445)).
- **Export validation:** Validate selected non-TLG exports and CDISC
  field types and lengths before saving, report custom-field validation
  limits, and block exports on validation or XPT-writing failures
  ([\#1452](https://github.com/pharmaverse/aNCA/issues/1452)).
- **CDISC metadata:** Include pre-specifications, session information,
  custom grouping variables, dose reference, route, and exclusion flags
  in exported datasets
  ([\#829](https://github.com/pharmaverse/aNCA/issues/829),
  [\#998](https://github.com/pharmaverse/aNCA/issues/998),
  [\#1077](https://github.com/pharmaverse/aNCA/issues/1077),
  [\#1141](https://github.com/pharmaverse/aNCA/issues/1141),
  [\#1276](https://github.com/pharmaverse/aNCA/issues/1276)).
- **CDISC corrections:** Report acceptance criteria with satisfied flags
  as `Y` and violations as blank, identify dose-aware methods through
  `PPANMETH`, and correct CORRXY and TMAX labels
  ([\#481](https://github.com/pharmaverse/aNCA/issues/481),
  [\#787](https://github.com/pharmaverse/aNCA/issues/787),
  [\#1102](https://github.com/pharmaverse/aNCA/issues/1102),
  [\#1242](https://github.com/pharmaverse/aNCA/issues/1242),
  [\#1332](https://github.com/pharmaverse/aNCA/issues/1332)).

### Help & compatibility

- **Help and navigation:** Added parameter references, ratio and
  R-script guides, contextual help, session information, clearer labels,
  and resizable sidebars
  ([\#899](https://github.com/pharmaverse/aNCA/issues/899),
  [\#975](https://github.com/pharmaverse/aNCA/issues/975),
  [\#1015](https://github.com/pharmaverse/aNCA/issues/1015),
  [\#1023](https://github.com/pharmaverse/aNCA/issues/1023),
  [\#1090](https://github.com/pharmaverse/aNCA/issues/1090),
  [\#1156](https://github.com/pharmaverse/aNCA/issues/1156),
  [\#1251](https://github.com/pharmaverse/aNCA/issues/1251),
  [\#1300](https://github.com/pharmaverse/aNCA/issues/1300),
  [\#1333](https://github.com/pharmaverse/aNCA/issues/1333)).
- **PKNCA compatibility:** Fixed development-version failures affecting
  start imputation, duplicate R2ADJ results, half-life selection,
  excretion parameters, and dose-escalation reports; R \>= 4.4 is now
  required ([\#1102](https://github.com/pharmaverse/aNCA/issues/1102)).
- **App startup:** Fixed installed-package and read-only deployment
  startup failures and recovery after NCA errors
  ([\#913](https://github.com/pharmaverse/aNCA/issues/913),
  [\#1107](https://github.com/pharmaverse/aNCA/issues/1107),
  [\#1378](https://github.com/pharmaverse/aNCA/issues/1378)).
- **Optional dependencies:** Reporting packages `rlistings`, `officer`,
  and `flextable` are now optional, with notifications when required for
  an output ([\#1106](https://github.com/pharmaverse/aNCA/issues/1106)).

## aNCA 0.1.0

CRAN release: 2025-12-09

- Initial CRAN submission.
