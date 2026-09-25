---
name: documentation-safety
description: Keep aNCA roxygen, imports, NAMESPACE, DESCRIPTION, and generated manuals consistent before handoff.
---

# Documentation Safety

Use when changing exported functions, arguments, imports, file layout, or
package documentation. Use `ci-roxygen-triage` for an already-failing CI job.

1. Trace changed API signatures and imports to their roxygen source and
   `DESCRIPTION` dependency declaration.
2. Check `@param`, `@returns`, `@export`, examples, and import tags match the
   implementation. Do not edit `man/` as the source of truth.
3. Identify generated artifacts affected by `devtools::document()` and whether
   the current environment can run it; do not claim generation when unavailable.
4. Check `NAMESPACE`, `DESCRIPTION`, and generated manuals will agree after
   regeneration.

**Output:** source files checked, generated artifacts expected, command/CI
evidence, and required human follow-up. Link `ci-roxygen-triage` for failure
details.

Example: “Review documentation safety after adding an exported argument and an
imported function.”
