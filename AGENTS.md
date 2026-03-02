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

---

# Working with AI Agents

This section describes how to effectively work with AI agents (GitHub Copilot, Ona, etc.) on aNCA development tasks.

## What Agents Can Do

**File operations:**
- Read and edit code files
- Create new files and directories
- Perform multi-step code transformations

**Development tasks:**
- Review code and suggest improvements
- Identify CI failures and propose fixes
- Trace logic through the codebase
- Search for patterns or usages across files
- Refactor and rename code

**GitHub operations:**
- Review PR diffs and code changes
- Check CI status and test results
- Comment on PRs and issues
- Create and update documentation

**Verification:**
- Check if compiled/validation passes
- Verify code patterns match project conventions
- Validate syntax and formatting

## What Agents Cannot Do

**Cannot run R code:**
- No `devtools::document()`, `devtools::test()`, `devtools::check()`
- Cannot run Shiny app or test interactively

**Cannot run or debug Shiny:**
- Cannot see visual output from the app
- Cannot debug at runtime
- Cannot take screenshots (desktop only)

**GitHub limitations:**
- Cannot reply in inline review threads (posts top-level comments instead)
- Cannot approve PRs or merge branches
- No thread replies — posts top-level PR comments tagging reviewers

**Other constraints:**
- Cannot assume R is installed in dev container
- Cannot run package-specific commands like lintr, testthat directly
- No persistent memory between sessions — provide PR/issue numbers each time

## Common Tasks

### Ask Agent to Review a PR

```
Review PR #NNN
```

Agent will:
- Read the PR diff and title/description
- Check CI status and test results
- Identify style issues, missing tests, documentation gaps
- Flag findings with severity levels
- Provide information about the findings (question for clarification, import fix to do, potential bug), with suggestions on what the developer should do next.
- NOT post any comments on the PR or push anything, unless the user specifies it.
- NOT comment on CI/CD failure status.
- NOT flag minor stylistic nitpicks (e.g., whitespace, trailing commas) that do not impact readability or execution.
- NOT comment on the changes that have been merged from main and that are not covered in the PR.

### Ask Agent to Fix Review Findings

```
Fix findings 1, 3, 5
```

Or:

```
Fix all findings
```

Agent will:
- Address each specified finding
- Push commits (keeping fixes separate where possible) with user approval
- Post a PR comment summarizing changes with user approval

After agent fixes:
- Test the app yourself (agent cannot run R)
- Verify CI checks pass on the PR
- Review the diff before merging

### Ask Agent to Check CI Status

```
Why is CI failing on PR #NNN?
```

Agent will:
- Check the GitHub Actions logs
- Identify which check failed (lint, tests, etc.)
- Suggest fixes

### Ask Agent to Add Tests

```
Add tests for function X
```

Agent will:
- Create test file `tests/testthat/test-x.R`
- Match existing test patterns
- Include descriptive test names and assertions

### Ask Agent to Update Documentation

```
Update roxygen for function X
```

Agent will:
- Add/update `@param`, `@returns`, `@examples`
- Regenerate man pages (if R available)
- Ensure examples are runnable

### Ask Agent to Trace Code Logic

```
Trace the zip export for scenario X
```

Agent will:
- Map data flow through the code
- Identify which functions are called
- Show the output at each step

### Ask Agent to Refactor/Rename

```
Rename function X to Y across the codebase
```

Agent will:
- Update function definition
- Update all calls to the function
- Update roxygen docs
- Update tests

## Guidelines for Asking Agents

### Be Specific

- Always provide PR/issue numbers: "Review PR #123"
- Specify which findings to fix: "Fix 1, 2, 4" not "Fix everything"
- Name the exact function or file: "Add tests for positive_mean in R/positive_mean.R"

### Provide Context

- "This is part of the zip export workflow" (helps agent understand scope)
- "Follow the pattern in tab_data_server" (reference examples)
- Link to related issues or PRs for background

### Split Large Tasks

- Instead of "Fix the whole PR," ask: "Fix CI lint errors" then "Add missing tests"
- Instead of "Refactor tab_nca," ask: "Extract zip filename logic to helper function"
- This lets you review each change independently

### After Agent Work

- Always review the git diff before committing
- Test functionality agent cannot run (Shiny app, R commands)
- Ask agent to adjust if changes don't meet expectations
- Don't skip testing just because agent cannot run it

## Common Mistakes

- "Fix everything" — too vague, agent will guess. Use "Fix findings 1, 3, 5"
- Skip testing Shiny changes — agent cannot see the UI. Test yourself after agent edits Shiny code
- Ask "Can you?" or "Should I?" — use direct requests. "Add tests for X" or "Rename X to Y"
- Assume agent remembers your last 10 questions. Include PR number: "Review PR #123" every time

## Agent Workflow Summary

For a typical PR cycle:

1. **You implement** — Create branch and code locally
2. **Ask review** — "Review PR #NNN"
3. **Agent finds issues** — Lists 3-6 findings with severity
4. **You decide** — Which fixes to ask about
5. **Ask fixes** — "Fix 1, 3, 5" (or "Fix all")
6. **Agent fixes** — Commits and pushes
7. **You test** — Run Shiny app, user workflows
8. **Check status** — "Are CI checks passing?"
9. **Review diff** — Before merging
10. **Merge** — Core team merges when approved

This keeps agent focused on what it does best (code analysis, edits, verification) while you focus on testing and design decisions.
