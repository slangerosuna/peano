# Positioning and Pillars Plan

## Overview
Peano's foundational positioning centers on delivering Rust-like semantics with first-class support for ECS, GPU, web/networking, and advanced numerics. This plan captures the enduring pillars that guide language and standard library development.

## Guiding Pillars
- **Rust-ish core semantics**
  - Expressions, traits, zero-cost abstractions, and strong static types should feel familiar to Rust developers while staying idiomatic to Peano.
  - **Metric:** 90% of core language examples should translate from Rust with ≤20% code adjustment.
- **First-class domains**
  - ECS/in-memory RDBMS, GPU/compute shaders, web/networking, and matrix/numerics capabilities must be incorporated directly into language constructs and standard library batteries rather than bolted-on libraries.
  - **Metric:** Each Tier-1 domain must have at least two flagship samples in `examples/` and integration coverage in CI.
- **Practical traits and utilities**
  - Provide pragmatic traits, panic semantics, and utilities that make day-to-day development approachable, predictable, and performant.
  - **Metric:** Developer survey satisfaction ≥4.5/5 and <10 open issues tagged `ergonomics` for >30 days.

## Next Steps
- **Codify success metrics** for each pillar to inform future roadmap prioritization (owner: PM team, due next sprint).
- **Propagate pillar language** into documentation and onboarding materials; create `docs/pillars.md` reference.
- **Review roadmap alignment** quarterly to ensure initiatives respect the core positioning; publish summary notes.
- **Establish positioning scorecard** capturing qualitative feedback from community channels.

## Milestones
- **M1 — Metrics Draft (2 weeks):** agree on KPIs, instrument CI checks, update documentation.
- **M2 — Communication Refresh (2 weeks):** update onboarding, marketing materials, examples to reflect pillars.
- **M3 — Quarterly Review Loop (1 week):** schedule recurring reviews, publish first scorecard.

## Pseudocode and Complexity

### Positioning Scorecard Pipeline
```pn
generate_scorecard :: () -> Scorecard => {
	surveys := collect_survey_results()
	telemetry := fetch_usage_metrics()
	issues := query_issue_tracker("ergonomics")
	score := aggregate(surveys, telemetry, issues)
	publish(score)
	ret score
}
```
- **Time:** O(S + T + I) where S survey responses, T telemetry records, I issue count.
- **Space:** O(S + T + I) temporary storage for aggregation.
