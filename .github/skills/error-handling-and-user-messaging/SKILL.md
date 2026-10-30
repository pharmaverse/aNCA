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
4. Classify the expected response as a blocking error, warning, recoverable
   validation message, or log-only internal failure, and identify the channel
   where it appears.
5. When a failure is unexpected, intermittent, unresolved, or may recur, add
   the smallest safe structured diagnostics at the boundary where evidence is
   lost. Include the execution path, relevant state, event, and correlation
   or session identifier; bound and redact the output, and never log sensitive
   data or complete datasets.
6. For a browser-facing loop or failure, use browser console/network evidence
   only when relevant and give the user exact steps for collecting it. Remove
   temporary diagnostics or retain only the safe, justified logging needed for
   ongoing support.
7. Verify valid behavior remains unchanged and add a negative regression case.

**Output:** failure path, blocked side effect, user message and channel,
diagnostic evidence plan, and test/workflow. Use `shiny-reactive-safety` for
reactive loops and `risk-analysis` for material records or exports.

Example: “Ensure invalid selected output blocks ZIP creation before any file is
written and tells the user what to correct.”
