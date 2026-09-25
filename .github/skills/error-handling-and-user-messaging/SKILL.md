---
name: error-handling-and-user-messaging
description: Review aNCA failure handling so unsafe processing stops and users receive actionable messages.
---

# Error Handling And User Messaging

Use when validation, data handling, calculation, save, export, or integration
failure behavior changes.

1. Trace the failure from invalid input/state to its first side effect and user
message. Check that unsafe calculation, record creation, or partial output
cannot continue.
2. Distinguish user-correctable data/configuration errors from internal failures
without exposing internals or sensitive data.
3. Make the message name the affected item and corrective action; preserve
diagnostic detail in appropriate logs/evidence where available.
4. Verify valid behavior remains unchanged and add a negative regression case.

**Output:** failure path, blocked side effect, user message, and test/workflow.
Use `risk-analysis` for material records or exports.

Example: “Ensure invalid selected output blocks ZIP creation before any file is
written and tells the user what to correct.”
