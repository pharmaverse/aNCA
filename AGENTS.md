# aNCA Development Guidelines

This is the single source of truth for all AI coding agents (GitHub Copilot, Ona, etc.) working on the aNCA project. All other agent configuration files reference this document.

## Repository Structure

- `R/` — Package R functions (exported)
- `inst/shiny/` — Shiny application code
- `inst/shiny/functions/` — Shared Shiny helper functions (app-only)
- `inst/shiny/www/styles/` — SCSS source files for styling
- `tests/testthat/` — Unit tests
- `man/` — Documentation (auto-generated, do not edit)
- `data/` — Package data
- `data-raw/` — Developer-time scripts (e.g., CSS compilation)

## Code Principles

### Simplicity

- Avoid deep nesting and unnecessary abstraction.
- Write the minimum code required to solve the issue.
- If something can be refactored to be simpler, do it.

### Reuse existing code

- Before writing new code, search the codebase for existing functions or patterns that already solve the problem.
- Check all files in `R/` and `inst/shiny/functions/` for existing helpers — not just `utils.R` files.
- Reuse or slightly refactor existing code rather than duplicating logic.
- Only write new code when nothing in the repo can be modified or reused.
- When a helper is used across multiple files, extract it into a dedicated utils file named by domain (e.g., `utils-slope_selector.R`, `utils-exclusions.R`).

## Where to put functions

- **`R/`** — Functions that can be used outside the Shiny app (package API, exported or internal).
- **`inst/shiny/functions/`** — Functions that are Shiny-specific and only used within the application.

Helper functions live alongside the code they support in both directories. When a helper is shared across multiple files, extract it into a domain-specific utils file (e.g., `utils-slope_selector.R`, `utils-exclusions.R`).

Before writing new code, check all files in both `R/` and `inst/shiny/functions/` for existing helpers.

## R Package Code (`R/`)

### Key constraints

- No `library()` or `require()` in package code
- All exported functions must have roxygen2 docs (`@param`, `@returns`, `@export`)
- Run `devtools::document()` after modifying roxygen comments
- Run `devtools::test()` before committing

### File naming

- One main function per file: `R/my_function.R`
- Match file name to function: `positive_mean()` → `positive_mean.R`
- Utilities: `R/utils.R` for small helpers

### Dependencies

- Check `DESCRIPTION` Imports/Suggests before using any package
- Avoid adding new dependencies when possible — prefer reusing packages already in Imports
- For new dependencies: `usethis::use_package("pkg")`

### Using external functions

- **Imports packages** (listed in `DESCRIPTION` Imports): use `@importFrom pkg fun` in the function's roxygen docstring and call `fun()` directly in code. Do not use `pkg::fun()` for Imports packages.
- **Suggests packages** (listed in `DESCRIPTION` Suggests): use `pkg::fun()` inline. Do not use `@importFrom` for Suggests packages.

When adding or changing `@importFrom` tags, agents must also manually update `NAMESPACE` (add/remove the corresponding `importFrom()` directive) and verify the package is listed in `DESCRIPTION` Imports. Developers can run `devtools::document()` to do this automatically.

### Documentation (roxygen2)

```r
#' Calculate mean of positive values
#'
#' @param x A numeric vector.
#' @param na.rm Logical. Remove NA values? Default `TRUE`.
#'
#' @returns A single numeric value.
#' @export
#'
#' @examples
#' positive_mean(c(-1, 2, 3, NA))
positive_mean <- function(x, na.rm = TRUE) {
  x <- x[x > 0]
  mean(x, na.rm = na.rm)
}
```

### Global variables (`R/zzz.R`)

Use for non-standard evaluation (NSE) column references:

```r
utils::globalVariables(c("DOSEA", "TRT01A", "GROUP"))
```

- Only for code in `R/`, not in `inst/shiny/`
- Keep alphabetically sorted, no duplicates

## Shiny App Code (`inst/shiny/`)

### Module communication

- `session$userData$results` — computed results (CDISC datasets, NCA results)
- `session$userData$project_name()` — reactive, auto-populated from STUDYID
- `session$userData$slope_rules()` — reactive (non-nested)
- Data flows: `data_upload_server` → `adnca_raw` → `tab_data_server` → `tab_nca_server`

### Export / ZIP (`tab_nca/zip.R`)

- `TREE_LIST` defines export structure
- `input$res_tree` returns selected items as text names
- Zip filename: `ProjectName.zip` or fallback `NCA_STUDYID.zip`
- Pre-Specs sheets auto-generated from CDISC data, filtered to exported variables

### Exploration plots

- Default color_by priority: `DOSEA > TRT01A > GROUP > ACTARM > COHORT`
- Default facet_by priority: `TRT01A > DOSEA > GROUP > ACTARM > COHORT`
- Auto-mapping includes: `COHORT`, `PART`, `PERIOD`

### CSS / SCSS workflow

Styles are authored in SCSS partials under `inst/shiny/www/styles/` and compiled into `inst/shiny/www/main.css`.

- **Entry point:** `styles/main.scss` imports all partials.
- **Partials:** `styles/partials/_base.scss`, `_sidebar.scss`, `_excretion.scss`, etc.
- **Color variables:** `styles/modules/_colors.scss`.

**For agents:** Edit the relevant `.scss` partial **and** apply the same change to `inst/shiny/www/main.css`. Agents cannot run the compile script, so both files must be updated manually to stay in sync.

**For developers:** Edit `.scss` files, then run `Rscript data-raw/compile_css.R` to recompile `main.css`.

**Sidebar width:** `main.css` sets `--_sidebar-width: 170px !important`. To change it, modify the value in `styles/partials/_sidebar.scss` (and `main.css`).

## Testing

### File structure

- Test file names match source: `R/foo.R` → `tests/testthat/test-foo.R`
- Use `describe` and `it` functions
- Include value-level assertions, not just structure checks
- Use the test data created in `tests/testthat/setup.R` where appropriate

Example:

```r
describe("g_lineplot: structure and arguments", {
  it("returns a ggplot object with individual labels", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      x_unit = "RRLTU",
      y_unit = "AVALU",
      color_by = "USUBJID"
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "PK Concentration - Time Profile")
    expect_equal(p$labels$y, "Concentration [ng/mL]")
    expect_equal(p$labels$x, "Time [hours]")
    expect_equal(p$labels$colour, "USUBJID")
  })

  it("applies faceting", {
    p <- g_lineplot(
      data = ind_data,
      x_var = "time_var",
      y_var = "AVAL",
      color_by = "USUBJID",
      facet_by = "PARAM"
    )
    expect_s3_class(p$facet, "FacetWrap")
  })
})
```

## CI Checks (GitHub Actions)

All PRs must pass:

- [ ] Code passes lintr checks
- [ ] Code passes all unit tests
- [ ] New logic covered by unit tests
- [ ] New logic is documented
- [ ] App or package changes are reflected in NEWS
- [ ] Package version is incremented
- [ ] R script works with the new implementation (if applicable)
- [ ] Settings upload works with the new implementation (if applicable)

## Working with Code

Before committing:
- `devtools::document()` — regenerate man pages and NAMESPACE
- `lintr::lint_package()` — check code style
- `devtools::test()` — run tests
- `devtools::check()` — full package check

## Issues

When creating issues, use the templates in `.github/ISSUE_TEMPLATE/`:
- **Bug report** (`BUG_REPORT.md`) — for reporting issues with existing features
- **Enhancement suggestion** (`ENHANCEMENT_SUGGESTION.md`) — for requesting new features

## PR Workflow

- **Before creating a branch**: Always `git fetch origin main && git checkout origin/main` to ensure you branch from the latest remote main. Do not rely on the local `main` ref being up to date.
- **Branch name**: `<issue-number>-<type>/<short-description>` (e.g., `123-bug/data-upload-fails`)
- **Before submitting**: Run checks above, bump version (+1 compared to the main branch). Propose NEWS.md updates (with the # of the pull request) by adding information about features or bug fixes
- **PR template**: Use `.github/PULL_REQUEST_TEMPLATE.md`. Link issue with `Closes #<number>`, describe changes, complete the contributor checklist
- **Reviewers**: Add at least 2 core team members (from DESCRIPTION)
- **Merge**: Core team will merge once approved

## Anti-patterns

- Do not use `git push --force` or `git push --force-with-lease` — always use separate commits instead of amending and force-pushing
- Do not edit `man/` or `NAMESPACE` files manually
- Do not add unused globalVariables to `R/zzz.R`
- Do not push without running `devtools::document()` first
- Do not skip tests or lintr checks
- Do not assume R is available in dev container (it may not be)
- Do not edit `main.css` without updating the corresponding `.scss` source
- Do not duplicate existing code — reuse or refactor

## Agent Configuration

This repository uses the following agent configuration:

- `AGENTS.md` — This file. Single source of truth.
- `.github/copilot-instructions.md` — Copilot entry point (references this file).
- `.github/instructions/` — Path-specific notes for Copilot.
- `.github/agents/` — Role-specific agent profiles.
- `.github/skills/` — Reusable agent workflows.
