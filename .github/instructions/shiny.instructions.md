---
applyTo: "inst/shiny/**"
---

# Shiny App Code

## Module communication

- `session$userData$results` — computed results (CDISC datasets, NCA results)
- `session$userData$project_name()` — reactive, auto-populated from STUDYID
- `session$userData$slope_rules()` — reactive (non-nested)
- Data flows: `data_upload_server` → `adnca_raw` → `tab_data_server` → `tab_nca_server`

## Export / ZIP (`tab_nca/zip.R`)

- `TREE_LIST` defines export structure
- `input$res_tree` returns selected items as text names
- Zip filename: `ProjectName.zip` or fallback `NCA_STUDYID.zip`
- Pre-Specs sheets auto-generated from CDISC data, filtered to exported variables

## Exploration plots

- Default color_by priority: `DOSEA > TRT01A > GROUP > ACTARM > COHORT`
- Default facet_by priority: `TRT01A > DOSEA > GROUP > ACTARM > COHORT`
- Auto-mapping includes: `COHORT`, `PART`, `PERIOD`

## CSS caveat

`inst/shiny/www/main.css` has `--_sidebar-width: 170px !important`

To change sidebar width, modify the CSS variable, not the R parameter.

## Anti-patterns

- Do not add `globalVariables` here; place them only in `R/zzz.R`
- Agents cannot run or visually debug the Shiny app; test UI changes manually
