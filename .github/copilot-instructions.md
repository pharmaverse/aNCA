# aNCA — Copilot Instructions

This is an R package for automated Non-Compartmental Analysis (NCA) with a Shiny app interface.

## Repository Structure

- `R/` — Package R functions (exported)
- `inst/shiny/` — Shiny application code
- `tests/testthat/` — Unit tests
- `man/` — Documentation (auto-generated, do not edit)
- `data/` — Package data

## Key Constraints

- No `library()` or `require()` in package code under `R/`
- Use `pkg::function()` syntax for all external function calls
- All exported functions must have roxygen2 docs (`@param`, `@returns`, `@export`)
- Run `devtools::document()` after modifying roxygen comments
- Run `devtools::test()` before committing

## Dependencies

- Check `DESCRIPTION` Imports before using any package
- For new dependencies: `usethis::use_package("pkg")`
- Use explicit namespaces: `dplyr::filter(x > 0)` not `filter(x > 0)`

## Testing

- Test file names match source: `R/foo.R` → `tests/testthat/test-foo.R`
- Use `describe` and `it` functions
- Include value-level assertions, not just structure checks
- Use test data from `tests/testthat/setup.R` where appropriate

## Anti-patterns

- Do not edit `man/` or `NAMESPACE` files manually
- Do not add unused `globalVariables` to `R/zzz.R`
- Do not push without running `devtools::document()` first
- Do not skip tests or lintr checks

## PR Workflow

- Branch name: `<issue-number>-<type>/<short-description>`
- Before submitting: bump version, update NEWS.md
- Link issue with `Closes #<number>` in PR description
- Add at least 2 core team reviewers (from DESCRIPTION)

## CI Checks

All PRs must pass: lintr, unit tests, test coverage, man page generation, and spell check.

## Additional Context

See `AGENTS.md` for full development guidelines.
See `.github/instructions/` for path-specific coding conventions.
See `.github/skills/` for reusable agent workflows.
