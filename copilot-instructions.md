# aNCA — Copilot Instructions

R package for automated Non-Compartmental Analysis (NCA) with a Shiny
app interface.

**Read `AGENTS.md` at the repository root for the full development
guidelines.** It is the single source of truth for all conventions, code
principles, and anti-patterns.

## Quick Reminders

- No [`library()`](https://rdrr.io/r/base/library.html) or
  [`require()`](https://rdrr.io/r/base/library.html) in package code
  under `R/`
- Imports packages: use `@importFrom` in roxygen, call directly.
  Suggests packages: use `pkg::fun()` inline
- All exported functions must have roxygen2 docs (`@param`, `@returns`,
  `@export`)
- Test files match source: `R/foo.R` → `tests/testthat/test-foo.R`
- Edit both `.scss` source and `main.css` for styling changes (see
  AGENTS.md for details)
- Search for existing code before writing new code
- Keep code simple — avoid unnecessary nesting

## Path-Specific Instructions

See `.github/instructions/` for path-scoped conventions applied
automatically by Copilot.

## Agent Profiles

See `.github/agents/` for role-specific agent behavior.
