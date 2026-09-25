---
name: test-data-and-fixture-design
description: Design minimal representative aNCA test data and fixtures for focused behavior evidence.
---

# Test Data And Fixture Design

Use when a test needs data beyond existing repository fixtures.

1. Start with an existing fixture; add data only for the behavior it cannot
   represent.
2. Keep the fixture minimal and named for its scenario. Include only columns,
   records, units, and metadata needed to expose the behavior.
3. Add a valid baseline and one targeted edge: missing optional column, invalid
   type/value, boundary, duplicate, or domain-specific case.
4. Assert values or observable outcomes, not just object shape.

**Output:** fixture purpose, fields/records required, edge case, and focused
assertion. Link `mapping-and-data-contracts` for field rules and
`test-strategy-selection` for test level.

Example: “Create the smallest fixture with valid output plus one invalid ADNCA
field for export validation.”
