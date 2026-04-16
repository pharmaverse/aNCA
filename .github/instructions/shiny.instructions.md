---
applyTo: "inst/shiny/**"
---

# Shiny App Code

See `AGENTS.md` section "Shiny App Code" for full conventions including module communication, export/ZIP workflow, exploration plots, and CSS/SCSS workflow.

## Path-specific reminders

- Check all files in `inst/shiny/functions/` and `R/` for existing helpers before writing new code
- Data flows: `data_upload_server` → `adnca_raw` → `tab_data_server` → `tab_nca_server`
- For styling changes, edit both the `.scss` partial and `inst/shiny/www/main.css`
- Do not add `globalVariables` here; place them only in `R/zzz.R`
- Agents cannot run or visually debug the Shiny app; flag UI changes for manual testing
