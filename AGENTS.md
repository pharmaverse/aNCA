# aNCA Development Guidelines

## Quick Reference

Repository structure:
- `R/` — Package R functions (exported)
- `inst/shiny/` — Shiny application code
- `tests/testthat/` — Unit tests
- `man/` — Documentation (auto-generated, do not edit)
- `data/` — Package data

Key constraints:
- No `library()` or `require()` in package code
- Use `pkg::function()` syntax for external functions
- All exported functions must have roxygen2 docs (`@param`, `@returns`, `@export`)
- Run `devtools::document()` after modifying roxygen comments
- Run tests before committing: `devtools::test()`

## R Package Code (`R/`)

### File naming

- One main function per file: `R/my_function.R`
- Match file name to function: `positive_mean()` → `positive_mean.R`
- Utilities: `R/utils.R` for small helpers

### Dependencies

- Check `DESCRIPTION` Imports before using any package
- For new dependencies: `usethis::use_package("pkg")`
- Use explicit namespaces in code: `dplyr::filter(x > 0)` not `filter(x > 0)`

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

### CSS caveat

`inst/shiny/www/main.css` has `--_sidebar-width: 170px !important`

To change sidebar width, modify the CSS variable, not the R parameter.

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

## PR Workflow

- **Branch name**: `<issue-number>-<type>/<short-description>` (e.g., `123-bug/data-upload-fails`)
- **Before submitting**: Run checks above, bump version (+1 compared to the main branch). Propose NEWS.md updates (with the # of the pull request) by adding information about features or bug fixes
- **PR template**: Link issue with `Closes #<number>`, describe changes, complete checklist
- **Reviewers**: Add at least 2 core team members (from DESCRIPTION)
- **Merge**: Core team will merge once approved

## Anti-patterns

- Do not edit `man/` or `NAMESPACE` files manually
- Do not add unused globalVariables to `R/zzz.R`
- Do not push without running `devtools::document()` first
- Do not skip tests or lintr checks
- Do not assume R is available in dev container (it may not be)
