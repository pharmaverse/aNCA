---
name: agent-workflow
description: >
  How to work with GitHub Copilot agents (Ona) in the aNCA development workflow.
  Use when asking agents to review PRs, fix issues, or handle tasks.
---

# Working with Agents (GitHub Copilot)

This guide describes how to effectively work with GitHub Copilot agents on aNCA development tasks.

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

## Reference

See `AGENTS.md` for aNCA development conventions and code requirements.
