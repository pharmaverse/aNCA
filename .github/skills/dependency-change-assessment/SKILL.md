---
name: dependency-change-assessment
description: Assess an aNCA R dependency addition or upgrade before implementation or release.
---

# Dependency Change Assessment

Use when adding, upgrading, constraining, or relying on changed behavior from a
dependency.

1. Identify the current declared/installed version, proposed version, affected
   APIs, and user-facing or scientific behavior at risk.
2. Read upstream NEWS, source, tests, and relevant issues/PRs; do not infer
   behavior from a version number alone.
3. Check DESCRIPTION/NAMESPACE implications, CI support, compatibility range,
   and an upgrade/defer decision with a linked issue if needed.
4. Choose focused regression and release evidence.

**Output:** change rationale, affected paths, compatibility decision, evidence,
and follow-up. Use `pknca-compatibility` for PKNCA and
`release-version-review` for release readiness.

Example: “Assess whether a new PKNCA release changes interval or result-column
behavior used by aNCA.”
