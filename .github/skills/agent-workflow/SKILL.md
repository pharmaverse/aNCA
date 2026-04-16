---
name: agent-workflow
description: >
  How to work with AI coding agents in the aNCA development workflow.
  Use when asking agents to review PRs, fix issues, or handle tasks.
---

# Working with Agents

This guide describes how to effectively work with AI coding agents (GitHub Copilot, Ona, etc.) on aNCA development tasks.

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

**GitHub limitations:**
- Cannot reply in inline review threads (posts top-level comments instead)
- Cannot approve PRs or merge branches
- No persistent memory between sessions — provide PR/issue numbers each time

## Common Tasks

### Review a PR

```
Review PR #NNN
```

Agent will:
- Read the PR diff and title/description
- Check CI status and test results
- Identify style issues, missing tests, documentation gaps
- Flag findings with severity levels
- NOT post comments or push unless explicitly asked
- NOT flag minor stylistic nitpicks
- NOT comment on changes merged from main

### Fix Review Findings

```
Fix findings 1, 3, 5
```

Or:

```
Fix all findings
```

Agent will:
- Address each specified finding
- Push commits with user approval
- Post a PR comment summarizing changes with user approval

After agent fixes:
- Test the app yourself (agent cannot run R)
- Verify CI checks pass on the PR
- Review the diff before merging

### Check CI Status

```
Why is CI failing on PR #NNN?
```

Agent will:
- Check the GitHub Actions logs
- Identify which check failed (lint, tests, etc.)
- Suggest fixes

### Add Tests

```
Add tests for function X
```

Agent will:
- Create test file `tests/testthat/test-x.R`
- Match existing test patterns
- Include descriptive test names and assertions

### Update Documentation

```
Update roxygen for function X
```

Agent will:
- Add/update `@param`, `@returns`, `@examples`
- Ensure examples are runnable

### Trace Code Logic

```
Trace the zip export for scenario X
```

Agent will:
- Map data flow through the code
- Identify which functions are called
- Show the output at each step

### Refactor/Rename

```
Rename function X to Y across the codebase
```

Agent will:
- Update function definition
- Update all calls to the function
- Update roxygen docs
- Update tests

## Guidelines

### Code Principles

Agents should follow the principles in `AGENTS.md` during development and review:

- **Simplicity:** Avoid unnecessary nesting and abstraction. Write the minimum code needed.
- **Code reuse:** Search all files in `R/` and `inst/shiny/functions/` for existing helpers before writing new code.
- **CSS/SCSS:** Edit both `.scss` source and `main.css` (agents cannot run the compile script).

### Be Specific

- Always provide PR/issue numbers: "Review PR #123"
- Specify which findings to fix: "Fix 1, 2, 4" not "Fix everything"
- Name the exact function or file: "Add tests for positive_mean in R/positive_mean.R"

### Provide Context

- "This is part of the zip export workflow"
- "Follow the pattern in tab_data_server"
- Link to related issues or PRs for background

### Split Large Tasks

- Instead of "Fix the whole PR," ask: "Fix CI lint errors" then "Add missing tests"
- Instead of "Refactor tab_nca," ask: "Extract zip filename logic to helper function"

### After Agent Work

- Always review the git diff before committing
- Test functionality agent cannot run (Shiny app, R commands)
- Ask agent to adjust if changes don't meet expectations

## Typical PR Cycle

1. **You implement** — Create branch and code locally
2. **Ask review** — "Review PR #NNN"
3. **Agent finds issues** — Lists findings with severity
4. **You decide** — Which fixes to request
5. **Ask fixes** — "Fix 1, 3, 5" (or "Fix all")
6. **Agent fixes** — Commits and pushes
7. **You test** — Run Shiny app, user workflows
8. **Check status** — "Are CI checks passing?"
9. **Review diff** — Before merging
10. **Merge** — Core team merges when approved

## References

- `AGENTS.md` — Single source of truth for all guidelines
- `.github/agents/` — Role-specific agent profiles
