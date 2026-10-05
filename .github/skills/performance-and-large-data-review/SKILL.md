---
name: performance-and-large-data-review
description: Review aNCA changes for proportionate performance and large-data risk.
---

# Performance And Large Data Review

Use when a change can repeat calculations, reactive work, rendering, joining,
exporting, or retaining large data.

1. Identify data size/profile shape, repeated trigger, expensive path, and
user-visible delay or memory risk.
2. Check whether work can be cached, narrowed, deferred, or performed once
without changing correctness or state ownership.
3. Avoid speculative optimization; make the smallest change supported by the
path and expected workload.
4. Define proportionate evidence: realistic manual timing/interaction or a
focused regression assertion when possible.

**Output:** costly path, expected workload, chosen mitigation or rationale for
none, and verification. Link `shiny-reactive-safety` for reactive work.

Example: “Review whether changing a plot control reruns NCA for every profile.”
