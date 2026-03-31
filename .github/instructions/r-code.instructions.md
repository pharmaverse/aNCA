---
applyTo: "R/**/*.R"
---

# R Package Code

See `AGENTS.md` sections "R Package Code" and "Code Principles" for full conventions.

## Path-specific reminders

- One main function per file, name matches function
- Use `pkg::function()` for all external calls
- All exported functions need `@param`, `@returns`, `@export`
- NSE column references go in `R/zzz.R` (alphabetically sorted, no duplicates)
- Run `devtools::document()` after modifying roxygen comments
