# Changelog

All notable changes to this app. Versions follow [Semantic Versioning](https://semver.org/):
MAJOR for a change to the model or its notation, MINOR for new features
(a stage, a worked example, a figure), PATCH for fixes and wording.
Each release is tagged in git as `vX.Y.Z` and shown in the app footer.

## [1.0.1] - 2026-09-28

### App
- The In Words tab lays out its three columns at fixed widths, so an
  equation no longer collapses to one term per line beside its note.
- The preset card no longer doubles the word "Stage" in front of a stage
  name that already carries it.

## [1.0.0] - 2026-09-28

First public release as a standalone repository.

### Model
- Solow model in continuous time with a Cobb-Douglas technology, following
  Romer (2019) ch. 1: closed-form steady state, golden rule and speed of
  convergence; the transition by Euler steps of one year.
- The 45-degree diagram as one Euler step over h years, with the crossing
  found numerically; h cancels out of the steady state.
- Extension beyond the textbook: a government layer from Part 2 question 7
  of the ECON42550 past exam, in which spending enters only through the
  effective saving rate s_eff = s - (1 - lambda) sigma + phi sigma.

### App
- Six stages that add one layer of the model at a time.
- Nine worked examples, each belonging to one stage and shown only there.
- Equations, Notation and In Words tabs that track the model at each stage.
- Nine figures: the Solow diagram, the transition, the 45-degree diagram,
  the staircase, the shift figure comparing two economies, growth over time,
  consumption against the saving rate, growth accounting, and steady-state
  capital against government spending.
- Ghost curves showing the loaded worked example alongside the live sliders.
- Readout tiles for k*, break-even investment, growth, the golden rule, the
  half-life of convergence and the government verdict.
