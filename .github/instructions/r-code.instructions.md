---
applyTo: "R/**/*.R"
---

# R Package Code

See `AGENTS.md` sections "R Package Code" and "Code Principles" for full conventions.

## Path-specific reminders

- One main function per file, name matches function
- Imports packages: use `@importFrom` in roxygen, call directly. Suggests packages: use `pkg::fun()` inline
- When adding `@importFrom`, also update `NAMESPACE` and check `DESCRIPTION` Imports
- All exported functions need `@param`, `@returns`, `@export`
- NSE column references go in `R/zzz.R` (alphabetically sorted, no duplicates)
- Run `devtools::document()` after modifying roxygen comments
