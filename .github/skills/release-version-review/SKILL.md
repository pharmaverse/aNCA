---
name: release-version-review
description: Review a release candidate for version, NEWS, compatibility, and evidence readiness.
---

# Release Version Review

Use when preparing a release, not for ordinary development-process-only PRs.

1. Compare the release branch with the prior released version; list user-facing
   changes, fixes, removals, dependencies, and deferred work.
2. Check the package version, NEWS entries, breaking-change/migration notes,
   PKNCA compatibility, and relevant settings/output compatibility.
3. Collect CI, focused tests, manual Shiny checks, and SAT/validation evidence
   proportionate to risk. Mark unavailable evidence rather than assuming it.
4. Report release-ready, blocked, or follow-up-needed with the smallest next
   action.

**Output:** release checklist with evidence links and open risks. Use
`documentation-safety`, `pknca-compatibility`, and
`backward-compatibility-review` when applicable.

Example: “Review whether this version can be released after a settings-format
change and a PKNCA upgrade.”
