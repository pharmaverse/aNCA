---
applyTo: "tests/**"
---

# Testing

See `AGENTS.md` section "Testing" for full conventions and examples.

## Path-specific reminders

- Test file names match source: `R/foo.R` → `tests/testthat/test-foo.R`
- Use `describe` and `it` functions
- Include value-level assertions, not just structure checks
- Use test data from `tests/testthat/setup.R` where appropriate
- Run tests with: `devtools::test()`
