---
name: export-integrity-review
description: >
  Review aNCA ZIP export changes to preserve valid exports, prevent partial
  output, and provide focused evidence for selected artifacts.
---

# Export Integrity Review

Use for changes to ZIP export, selected output artifacts, export validation,
filenames, or generated export files. Do not use for unrelated plotting or
calculation changes.

## Review

1. Trace the selected artifact from `zip_server()` through validation,
   `prepare_export_files()`, file writing, and `zip::zipr()`.
2. Check that all selected outputs are validated before the first export file is
   written. A failed validation must produce no ZIP and no partial artifacts.
3. Check valid selected outputs are present, named as expected, and contain only
   intended files/variables.
4. Check invalid input, empty selection, writer failure, and repeat export have
   actionable user behavior and do not retain stale temporary output.
5. Preserve existing filename and ZIP-structure behavior unless the requirement
   explicitly changes it.

## Verify

- For an app-flow change, give the real Shiny workflow: fixture/data state →
  export selection → user action → expected ZIP or blocking message.
- Add or update the smallest regression case in
  `tests/testthat/test-zip-utils.R`, including one valid and one invalid
  selected output where atomicity is relevant.
- If R is unavailable, do not install it; inspect source/tests and use CI or
  state the manual app check needed.

## Output

Report:

- selected export path and validation gate;
- valid and invalid scenarios checked;
- expected user-visible result;
- regression test or manual workflow;
- any remaining risk.

Use `error-handling-and-user-messaging` for message wording and
`risk-analysis` when exported records or validation evidence are materially
affected.

Example: “Review whether selecting one valid output and one invalid ADNCA
output can leave export files behind after validation fails.”
