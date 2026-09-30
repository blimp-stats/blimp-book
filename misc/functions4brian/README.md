# rblimp diagnostic plots — handoff bundle

`distribution_plot()` and `residuals_plot()` build multiple-imputation diagnostic
graphics for a fitted rblimp model, plus the tests that pin their behavior.
Everything needed is in this folder. There is no separate design document on
purpose: the contract lives in the code, so it cannot drift out of sync. This
file is only a map.

## Start here
Open `rblimp_cleaned_functions.R` and read the header block first. It is the
**Design Contract** — the multiple-imputation pooling rules, the list of things
that are deliberately *not* plotted, and which parameters are fixed-by-design
vs. user-tunable. Per-function documentation (every argument, its default, and
the full catalog of returned panels) is the roxygen block above each of
`distribution_plot` and `residuals_plot`; those become `?distribution_plot` /
`?residuals_plot` once packaged.

## What each file is
- `rblimp_cleaned_functions.R` — the two exported functions + internal
  (`.`-prefixed) helpers. Self-contained; needs only ggplot2 + stats.
- `run_tests.R` — runs the synthetic regression suite standalone.
- `tests/testthat/` — the package test suite (no Blimp, no data needed):
  - `helper-make_test_model.R` — builds synthetic rblimp-style model objects,
    one per outcome/model branch. Its header doubles as the **model-object
    interface spec**: exactly which `@`-slots and column names the code reads.
  - `test-diagnostics.R` — 59 assertions that fail the build if a Design-Contract
    decision is changed (dispatch by outcome type, Q-Q suppression, pooled-sigma
    Cook's D, MLM/SEM level-2 routing, group-mean-centered display, …).
- `integration/` — real-Blimp end-to-end check (needs Blimp installed + internet
  for the book's public data):
  - `expected_panels.R` — the expected-panel manifest + `check_panels()`.
  - `integration_checks.R` — fits the four representative models (continuous,
    binary logistic, random-intercept MLM, latent-variable MLM-SEM) on the real
    data and verifies the returned panels match the manifest.

## How to run it
Synthetic suite (fast, no Blimp; needs ggplot2 + testthat):

    cd functions4brian
    Rscript run_tests.R            # expect 59 passing assertions, 0 failures

Real-Blimp integration (optional, slower; needs Blimp + rblimp):

    # edit integration/integration_checks.R: set_blimp("<your Blimp path>")
    Rscript integration/integration_checks.R   # prints PASS/FAIL per model

## Notes for the package port
- Dependencies: `Imports: ggplot2, stats`; `Suggests: ggrepel` (outlier labels,
  already guarded by `requireNamespace`). A `utils::globalVariables(...)` call is
  declared near the top of the functions file so R CMD check is clean.
- `tests/testthat/` drops straight into the package as-is. `run_tests.R` and
  `integration/` are developer tools, not package code.
- The returned list-element names (e.g. `rp$dpdd.index`, `dp$agetryalc`) are the
  public interface — the book's figure scripts and user code index the result by
  name, so the naming scheme should stay stable across versions.
