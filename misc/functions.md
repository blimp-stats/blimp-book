# rblimp Functions Cleanup — Change Log

This file tracks the cleanup decisions made while refactoring the rblimp
plotting functions for production handoff. Attach this file at the start of
any new conversation to bring Claude up to speed on what has been decided
and what has been changed.

## Project context

- Source file: `functions.R` (~5,000 lines, ~12 top-level functions).
- Goal: production-ready code suitable for handoff to a programmer who
  will integrate the functions into the rblimp package.
- Approach: in-place refactoring, function by function, rather than rewrite
  from scratch. Each pass cleans one function and its internal helpers.
- Cleaned functions are accumulated in `rblimp_cleaned_functions.R` in this
  same folder.

## Global decisions (apply to all functions)

- **No package globals.** The original file had `plot_colors`,
  `plot_point_color`, `plot_curve_color`, `plot_band_color`,
  `plot_fill_color`, `plot_shading`, `plot_band_coverage`, and
  `plot_jitter_fraction` as top-level objects. These are being removed.
  Per-function arguments replace them with sensible hex defaults.
- **Functions return ggplot objects with auto-print on the list class.**
  Multi-plot functions return a named list with class `c("<funcname>", "list")`
  and an S3 `print` method that auto-displays each plot at the console.
  Assignment is silent.
- **Default verbosity is silent.** Functions take a `verbose` argument
  defaulting to `FALSE`. Diagnostic and informational messages are gated
  on this flag. Errors and skip-warnings still surface.
- **Roxygen documentation is mandatory.** Every user-facing function gets
  `@param`, `@return`, `@examples`, and either `@export` or
  `@keywords internal`.
- **Input validation up front.** Functions verify the model is the right
  S4 class, that required slots exist, and that user-supplied vectors
  reference valid variables, before doing any work.
- **Decomposition target: ~150 lines per function.** Anything longer
  gets split into single-purpose internal helpers prefixed with `.`.
- **First positional argument is `model`** for all plotting functions
  that accept a model. This lets `f(my_model)` work naturally without
  argument-order hacks.
- **Standard color palette: rblimp posterior_plot defaults.** Plot
  functions inherit blue (`#00B0F6`), coral (`#F8766D`), and mint
  (`#00BF7D`) from the ggplot2 hue palette that posterior_plot already
  uses. This ensures visual consistency across the package.

## API conventions across functions

- Color arguments use suffix form: `observed_color`, `imputed_color`,
  `density_color`, `point_color`, `curve_color`, `band_color`,
  `fill_color`. Defaults are hex codes; users may pass any R color name
  or hex code.
- `font_size` defaults to 14.
- `fill_alpha` defaults to 0.25 for filled bar layers.
- Legend position and styling are part of the function's theme; not
  user-configurable in the function signature (override via standard
  ggplot `+ theme(...)` if desired).

## Function-by-function changelog

### `residuals_plot` — warn on few-valued continuous predictors (2026-06-17, fourteenth pass)

A categorical predictor left out of the `ORDINAL`/`NOMINAL` declarations is
modeled as continuous, and the residual-vs-predictor panel then fits a loess
smoother to a predictor with very few distinct values, producing cryptic
singular-fit warnings ("pseudoinverse used", "reciprocal condition number 0").
Those warnings were the only hint that a variable had been mis-declared.

`.residuals_vs_predictor` now checks the number of distinct values of any
predictor modeled as continuous and, when it is at or below a threshold, issues
a clear `warning()` naming the variable and recommending it be declared
`ORDINAL`/`NOMINAL` and re-fit. The threshold is a new `residuals_plot`
argument, `few_values_warn` (default `5`; `0` disables).

Design decision (per author): the function **only warns** and leaves the plot
exactly as specified. An earlier draft auto-switched few-valued predictors to
the discrete display, but altering the plot on an inference about the user's
intent is the wrong default — whether a sparse predictor should be treated as
categorical is the user's call. The guard changes nothing in the model itself
(the variable was still estimated as continuous); the substantive fix remains
re-declaring and re-running the model.

### `residuals_plot` — Unicode dash in loading arrow (2026-06-17, thirteenth pass)

Leverage and Cook's D were still coming out flat (intercept-only, `h = 1/n`)
for the indicators of a latent factor, even after the twelfth-pass fix. Cause:
the measurement-model loading lines in the book's scripts use a Unicode
**en dash** (`–>`, U+2013) rather than an ASCII hyphen (`->`), because an
editor auto-converted the hyphen. Blimp accepts the en-dash and fits the model
fine, but `.residuals_plot_predictors` matched only the ASCII `->`, so the
loading line was skipped and the indicators got no predictor.

Fix: in `.residuals_plot_predictors`, normalize Unicode dashes
(`‒–—―−`) to ASCII `-` before parsing, so `–>`,
`—>`, etc. are all recognized. Also drop trailing-colon block-label tokens
(e.g., `measurement:`) from the loading line's left-hand side so they aren't
mistaken for predictors. Verified on the Ch.6 mediation model: `il6` now
resolves to predictor `inflammation` (→ `inflammation.latent`), giving a
two-column design matrix and non-degenerate leverage.

### `residuals_plot` — latent-variable predictors (2026-06-14, twelfth pass)

Leverage and Cook's D came out degenerate (a flat line at `1/n`, with the
`2p/n` / `3p/n` reference lines floating above the points) whenever a DV's
only predictor was a latent factor — e.g., the indicators in a CFA
measurement model `f -> y1 y2 y3 y4`. Two compounding causes, both fixed:

1. **`->` (loading) syntax was not parsed.** `.residuals_plot_predictors`
   split MODEL chunks on `~` only, so loading lines (which use `->`, with
   the factor on the left and the indicators on the right — the reverse of
   `~`) were skipped and the indicators got an empty predictor list. The
   parser now handles `->`: each RHS outcome is assigned the LHS factor(s)
   as predictors. Inline `#` comments are stripped before tokenizing.

2. **Latent predictors live in `<name>.latent`.** Blimp imputes the factor,
   so the averaged data has `inflamlat.latent`, not a bare `inflamlat`
   column. A new `.resolve_predictor_col(data, pred)` helper maps a bare
   predictor name to its column (falling back to `<pred>.latent`), and every
   predictor-column lookup now routes through it: the main-loop guards,
   `.residuals_vs_predictor`, `.binned_residuals_vs_predictor`,
   `.predicted_probability_vs_predictor`, `.pearson_residuals_vs_predictor`,
   and `.build_design_matrix`.

Net effect: indicators in a measurement model now get a residual-vs-predictor
plot against the factor and a meaningful leverage / Cook's D (computed on the
imputed factor score, p = 2). This treats the imputed latent score as an
ordinary continuous predictor — a diagnostic other SEM software can't produce
because it integrates the factor out rather than imputing it. Predictors that
resolve to no column (e.g., stray comment tokens) are silently skipped, so the
change is inert for the existing `~` regression examples in Chapters 2–4.

### `residuals_plot` — index plot dual thresholds (2026-05-04, eleventh pass)

The standardized residual index plot now shows dashed reference lines
at both `±2` and `±3` (was `±2` only). The labeling logic uses the
largest absolute threshold, so case row numbers are labeled only for
cases with `|z| >= 3` — limiting label clutter to the most extreme
cases while still showing the broader `±2` reference for context.

Default `index_thresholds` changed from `c(-2, 2)` to
`c(-3, -2, 2, 3)`.

### `residuals_plot` — threshold labels nudged left (2026-05-04, tenth pass)

Threshold labels (`2p/n`, `3p/n`, `4/n`, `1.0`) were anchored at `x = 0`
with `hjust = 0`, which put the label text *to the right* of x = 0.
Changed to `hjust = 1` so the label's right edge sits at x = 0 and the
text extends left into the panel margin (where ggplot's default 5%
expansion provides space). Labels now sit closer to the y-axis,
visually clear of the data points.

### `residuals_plot` — label sizing and positioning (2026-05-04, ninth pass)

Three adjustments after rendering at book scale:

1. *Threshold-line labels moved to the left side* (`x = 0, hjust = 0`).
   The right-edge placement put the label too close to the densest
   data points and risked overlapping with case labels in the upper
   right. Anchoring at `x = 0` keeps the label clear of all data
   (since case indices start at 1) and reads cleanly above each
   threshold line.

2. *Threshold and outlier label sizes increased* to `font_size / 2.85`
   (was `font_size / 4` for thresholds, `font_size / 3` for outliers).
   The `2.85` divisor is the ggplot pt-to-mm conversion factor, so
   labels now render at the same physical size as the axis text. At
   default `font_size = 14`, labels are ~14pt — clearly readable in
   print.

3. *Theme aligned with `distribution_plot` conventions.* Changed
   `axis.text` to explicit `axis.text.x` and `axis.text.y` (matching
   the distribution_plot pattern), bumped `axis.title.x` margin from
   `t = 10` to `t = 12` (also matching), and dropped the
   `axis.title.y` right margin.

### `residuals_plot` — threshold-line labels (2026-05-04, eighth pass)

Added formula labels to the threshold lines on the leverage and Cook's D
index plots, anchored at the right edge of the plot just above each line.
The reader can now see at a glance what each line represents:

- Leverage plot: lines labeled `2p/n` (lower) and `3p/n` (upper).
- Cook's D plot: lines labeled `4/n` (lower) and `1.0` (upper).

The standardized residual index plot is left unlabeled because the
threshold values (e.g., ±2) are self-explanatory once the reader knows
the metric is standardized residuals.

Implementation: a single `ggplot2::annotate("text", ...)` call per plot,
positioned at `x = max(case)`, `y = cuts`, with `hjust = 1.0` and
`vjust = -0.3` so labels sit above each line at the right edge.

### `residuals_plot` — outlier labels on index plots (2026-05-04, seventh pass)

The case index on the X-axis of the index plots (standardized residual,
leverage, Cook's D) is unreadable at typical figure sizes, which made it
hard to identify which cases exceeded the threshold lines. Added
row-number labels for cases above the cutoff on each plot:

- Standardized residual index: labels for cases with `|z|` exceeding the
  largest threshold in `index_thresholds` (default ±2).
- Leverage: labels for cases above the upper cutoff (`3p/n`).
- Cook's D: labels for cases above the lower cutoff (`4/n`); the upper
  cutoff (`1.0`) is rarely exceeded in practice.

Implementation uses `ggrepel::geom_text_repel` if the package is
installed (clean non-overlapping placement) with a fallback to plain
`ggplot2::geom_text`. ggrepel becomes a Suggests-level dependency for
the rblimp package; the function works without it but with potentially
overlapping labels in dense cases.

A new internal helper `.add_outlier_labels(p, df, value_col, threshold,
style, two_sided)` consolidates the labeling logic across the three
index plots.

### `residuals_plot` — leverage and Cook's distance added (2026-05-04, sixth pass)

Added two new diagnostic plot families: leverage (diagonal of the hat
matrix) and Cook's distance. Both are index-style plots with horizontal
threshold lines.

**Computation**

Both diagnostics are computed without `lm()`. The implementation:

1. Builds the design matrix manually from the averaged imputations.
   Continuous, ORDINAL, COUNT, and binary NOMINAL predictors enter as-is
   (raw averaged values). NOMINAL predictors with > 2 distinct levels
   are dummy-expanded with the lowest level as the reference category.
2. Computes leverage as the diagonal of `X(X'X)^-1 X'` via
   `rowSums((X %*% XtX_inv) * X)`.
3. Computes Cook's D via
   `D_i = (e_i^2 / (p * sigma2)) * (h_ii / (1 - h_ii)^2)`, using
   Blimp's averaged residuals for `e` and `sum(e^2) / (n - p)` for
   `sigma2`.
4. Returns NULL (skipping the plots for that DV) if the design matrix
   is singular or has too few cases relative to parameters.

**Rationale for treating ORDINAL as raw codes**

ORDINAL predictors are treated as discrete in the regression in the
same way COUNT and continuous predictors are: the slope coefficient
applies to the raw codes. Only NOMINAL with > 2 levels requires dummy
expansion to avoid implying a linear ordering across categories.

**Helper additions**

- `.get_discrete_vars` extended to accept a `types` argument
  (`c("ordinal", "nominal", "count")`) so callers can pull declarations
  of a specific kind. Default behavior unchanged.
- `.build_design_matrix(data, predictors, nominal_vars)`: constructs
  the design matrix with NOMINAL dummy expansion.
- `.residuals_lm_diagnostics(data, dv, predictors, nominal_vars, verbose)`:
  computes both leverage and Cook's D via closed-form formulas.
- `.residuals_leverage(diag, dv, style)`: index plot with `2p/n` and
  `3p/n` reference lines.
- `.residuals_cooks(diag, dv, style)`: index plot with `4/n` and `1.0`
  reference lines.

**Caveats documented in the function docstring**

Cases with imputed predictor values have biased leverage and Cook's D
because the averaged X row sits closer to the predictor centroid than
any individual imputation would. Fully observed cases get correct
diagnostics; imputed cases are slightly underrated. This is the same
trade-off that applies to all averaged-imputation diagnostics in the
function.

### `distribution_plot` — production polish (2026-05-04, fourteenth pass)

Three small simplifications applied during a final review:

1. *Removed `subtitle <- NULL` line* in `.distribution_plot_one`. Since the
   percent-imputed indicator was folded into the title as a parenthetical,
   no subtitle is ever passed; the variable was vestigial.

2. *Bundled visual args into a `style` list* matching the pattern used in
   `residuals_plot`. `.distribution_plot_one`'s signature drops from 12
   args to 7. `.bar_layers`, `.density_layer`, and `.mixed_legend`
   helpers all take the `style` list instead of individual color/alpha/
   linewidth args. Internal API change only; user-facing function
   signature is unchanged.

3. *Simplified `.mixed_legend`'s value arrays.* Previously the function
   built four separate vectors (`fill_values`, `color_values`,
   `override_fill`, `override_color`) where the first two used `NA` for
   the Imputed fill (since the actual bars are outlined) and the latter
   two used the imputed color (for legend display). Since the dummy
   points driving the legend are invisible (alpha = 0), the scale's
   "natural" mapping doesn't visually matter — only override.aes does.
   Replaced with a single shared `legend_colors` vector that defines
   both the scale and the override.

The cleanup brings `distribution_plot`'s internal architecture into
alignment with `residuals_plot` and reduces total line count by ~30
without changing behavior.

### `residuals_plot` — production polish (2026-05-04, fifth pass)

Four cleanups applied:

1. *Removed `smoother` and `poly_degree` arguments.* The function now
   uses loess unconditionally. Users who want a polynomial fit on a
   specific plot can extend the returned ggplot via
   `+ geom_smooth(method = "lm", formula = y ~ poly(x, n))`. The
   smoother helper collapses to a single fixed-method `geom_smooth`
   call. Function signature drops from 12 args to 10.

2. *Updated stale roxygen text.* Removed mention of the "optional
   confidence band" (the band is no longer drawn under any
   circumstance) and changed "jittered points" to "stacked points at
   integer positions" to reflect the current rendering.

3. *Trimmed the lengthy Rubin-pooling paragraph* in the function's
   description down to one sentence. The detail is preserved but no
   longer dominates the docstring.

4. *Unified title formatting* across all three plot types to use
   "Residual" (singular). The residuals-vs-predicted title changed
   from `"Residuals vs. Predicted: <dv>"` to `"Residual vs. Predicted:
   <dv>"`. The other two were already singular.

### `residuals_plot` — discrete points and alpha (2026-05-04, fourth pass)

- *Jitter removed for discrete predictors.* The discrete-X branch now
  uses `geom_point` instead of `geom_jitter`, so points stack at exact
  integer positions on the X axis. The reader sees a column of
  overlapping points at each level, with the per-level mean diamond
  on top. Cleaner and more honest about the integer-valued nature of
  the X coordinate.
- *`point_alpha` default raised from 0.40 to 0.70.* The lighter alpha
  was washing out the blue points; 0.70 keeps the color saturation
  visible while still allowing some overlap-density signal.

### `residuals_plot` — streamline (2026-05-04, third pass)

Three simplifications:

1. *`ci`, `level`, `band_color`, `band_alpha` arguments removed.* With
   averaged imputations, the smoother's confidence band reflects only
   the smoother's fitting uncertainty, not imputation uncertainty. The
   band is no longer drawn under any circumstance. The smoother helper
   passes `se = FALSE` unconditionally. The function signature drops
   from 16 arguments to 12.

2. *Discrete predictors now use numeric X with integer breaks*, matching
   the pattern in `distribution_plot`. Previously the function converted
   discrete X to a factor, producing a categorical axis. The new
   approach (numeric X via `.integer_breaks`) handles binary, ordinal,
   and high-cardinality COUNT uniformly. The per-level mean diamond
   uses `aes(x = round(x))` so it sits at exact integer positions even
   when individual cases have non-integer averaged values.

3. *Predictor-plot title changed* to
   `"Residual vs. Predictor: <dv> ~ <iv>"` (was
   `"Residuals vs. <iv>: <dv>"`). The `~` separator mirrors regression
   syntax and clarifies which variable is the DV vs. predictor.

### `residuals_plot` — default tuning (2026-05-04, second pass)

After the first render:

- `ci` default flipped from `TRUE` to `FALSE`. The CI band on the smoother
  was a visual distraction; the diagnostic question (does the trend look
  flat or curved?) is answered by the smoother line alone. Users who
  want the band can set `ci = TRUE`.
- `point_color` default changed from `"#A3A500"` (olive) to `"#00B0F6"`
  (blue). Matches the rblimp palette's most prominent data color and
  contrasts cleanly with the coral smoother line.
- Added the same color reference comment to the function signature that
  `distribution_plot` carries:
  `# coral #F8766D olive #A3A500 mint #00BF7D blue #00B0F6 megenta #E76BF3`

Roxygen `@param` descriptions updated to reflect the new defaults.

### `residuals_plot` — completed (2026-05-04, supersedes original ~1500-line version)

**What this function does**

Builds three families of residual diagnostic plots for each outcome
variable in an rblimp model:

- *Residuals vs. predicted*: scatter + smoothed trend + optional CI band.
  Targets nonlinearity and heteroscedasticity in the structural Y–X
  relationship.
- *Residuals vs. each predictor*: one plot per predictor per DV.
  Continuous predictors get scatter + smoother. Declared discrete
  predictors (ORDINAL, NOMINAL, COUNT) get jittered points with the
  mean residual at each level.
- *Standardized residual index*: index plot of standardized residuals
  by case number with horizontal threshold lines (default ±2).

**Argument list**

```r
residuals_plot(
  model,
  vars             = NULL,
  smoother         = c("loess", "lm"),
  poly_degree      = 2,
  ci               = TRUE,
  level            = 0.95,
  font_size        = 14,
  point_color      = "#A3A500",
  curve_color      = "#F8766D",
  band_color       = "#F8766D",
  point_alpha      = 0.40,
  point_size       = 1.5,
  band_alpha       = 0.20,
  line_width       = 1.0,
  index_thresholds = c(-2, 2),
  verbose          = FALSE
)
```

Returns a named list of class `c("residuals_plot", "list")` with an S3
print method that auto-displays each plot at the console. Names follow
the pattern `"<dv>.predicted"`, `"<dv>.<predictor>"`, and `"<dv>.index"`.

**Behavior changes from the original**

- *Averaged imputations.* Each numeric column is averaged across the M
  imputed datasets before plotting, producing one residual, one
  predicted value, and one set of predictor values per case. The
  original used all M imputations stacked (M × N points). The averaged
  approach preserves the structural patterns these plots are designed
  to detect (linearity, heteroscedasticity, outliers) and dramatically
  simplifies the rendering.

- *No Rubin pooling.* The original computed Rubin-pooled polynomial
  regression with Barnard-Rubin degree-of-freedom adjustment, between-
  imputation variance accumulation, and lambda capping (~135 lines of
  pooling code). The cleaned version uses ggplot2's `geom_smooth`
  directly. The trade-off is that the confidence band around the
  smoother no longer reflects imputation uncertainty — but for a
  diagnostic visualization, the smoothed trend itself is what matters.

- *Sections D and E removed.* The original had five sections (A–E):
  residuals vs. predicted (A), residuals vs. predictors (B),
  standardized residual index (C), histograms of standardized
  residuals (D), and standardized residual spread by cluster (E). D
  was redundant with `distribution_plot` on `.residual` variables; E
  is multilevel-only and not exercised in Chapter 2. Both removed.
  If multilevel cluster diagnostics are needed in later chapters,
  they can be added as a focused extension at that time.

- *No cluster awareness.* The original handled random intercepts
  (`y[cluster]`), random slopes (`y$x[cluster]`), level-2 predictors,
  and three-level model parsing (~700 lines for Section B alone).
  None of this was exercised in Chapter 2 and it is removed in the
  cleaned version. A future cluster-aware extension would add a
  `cluster_id` argument and a focused branch.

- *Argument order: `model` is now first.* The original used a hack to
  detect when `vars` was passed a model object. Removed.

- *Removed arguments:* `point_alpha`, `support`, `trim_prop`,
  `window_prop`, `min_n_prop`, `center_by_imp`, `winsor_fit`,
  `winsor_k`, `signed_index`, `index_cutoff`, `index_aggregate`,
  `index_order`, `index_point_color`, `index_line_color`,
  `print_threshold`, `print_head`, `show_index`. These were either
  fitting-stability options needed only for the Rubin-pooled
  approach, or detail knobs for the diagnostic table that the cleaned
  version doesn't produce.

- *New arguments:* `smoother` (selects between loess and lm-poly),
  `index_thresholds` (vector of horizontal reference lines on the
  index plot).

**Structural changes**

- Decomposed into a 60-line user-facing function plus 8 internal
  helpers: `.residuals_plot_average`,
  `.residuals_plot_detect_dvs`, `.residuals_plot_predictors`,
  `.residuals_vs_predicted`, `.residuals_vs_predictor`,
  `.residuals_index`, `.residuals_smoother`,
  `.residuals_plot_theme`.
- Total length: ~430 lines (function + helpers + roxygen).
  Compare to ~1500 lines for the original.

**Behavior worth flagging for the programmer**

- The averaging approach means cases with imputed values for a
  predictor may have non-integer X coordinates on plots for declared
  discrete predictors (e.g., a binary `els` value of 0.65 for a case
  imputed across iterations). The function still treats `els` as
  discrete (per the `.get_discrete_vars` declaration) and renders it
  as a factor, but the factor includes the averaged levels. This is
  a feature: the reader sees imputation uncertainty in those cases.
  For a "purely categorical" rendering, the user can pre-process the
  averaged data to round binary values, but this is not the default.

### `distribution_plot` — line_width argument (2026-05-04, thirteenth pass)

The imputed histogram outline and density curve linewidths were
hardcoded at 1.0, which read as chunky relative to thinner axis
lines used in book/print figures. Added `line_width` argument
(default `1.0`, preserving previous behavior). For book figures with
`axis.line linewidth = 0.5`, values around `0.5–0.7` produce a
cleaner visual hierarchy.

The argument flows through `.distribution_plot_one` to both
`.bar_layers` (for the imputed histogram outline) and `.density_layer`
(for the density curve). Same value applies to both.

### `distribution_plot` — title shortened (2026-05-04, twelfth pass)

Mixed-case title was running off the page when the function was used
inside a patchwork combination at typical book figure dimensions.
Dropped the "Across N Imputations" portion. The variable name and the
percent-imputed indicator carry the essential information; the
iteration count was redundant with the same value being implicit in
the data.

Old: `"Observed vs. Imputed Distributions Across 20 Imputations: dpdd (12.1% imputed)"`
New: `"Observed vs. Imputed Distributions: dpdd (12.1% imputed)"`

The `n_imps` argument is still passed through `.distribution_plot_one`
because other places (the model-implied title path, future use) may
still need it. Only the mixed-case title format changed.

### `distribution_plot` — silenced bandwidth warning (2026-05-04, eleventh pass)

`stats::density()` issued a "Selecting bandwidth *not* using 'weights'"
warning whenever weighted density estimation was performed (for both
the y_upper helper and `geom_density()`). The warning was informational
— `density()` selects its bandwidth from the unweighted data when
weights are supplied, which is mathematically reasonable for our
weighting scheme but produces noisy console output.

Fix: compute the bandwidth explicitly via `stats::bw.nrd0()` on the
unweighted data and pass it as `bw =` to both call sites. Underlying
behavior is unchanged (same bandwidth as before, just no longer
auto-selected). The helper signatures for `.density_layer` and
`.distribution_plot_y_upper` gained a `bw` argument.

### `distribution_plot` — legend mirrors density suppression (2026-05-04, tenth pass)

For declared discrete variables (ORDINAL, NOMINAL, COUNT), the density
curve is suppressed in the plot but the "Combined" entry was still
appearing in the legend. Fixed by passing `show_density` through to
`.mixed_legend` and conditionally building either a three-entry
(Observed, Imputed, Combined) or two-entry (Observed, Imputed) legend
based on whether the density layer is drawn.

The legend now exactly mirrors what is shown in the plot: Combined
entry only appears when there is an actual density curve to label.

### `distribution_plot` — legend alpha tuning (2026-05-04, ninth pass)

The Imputed legend entry's alpha was set to `fill_alpha` (matching
Observed) but read as too washed-out — its corresponding plot element
is an outlined bar at full opacity, not a filled bar. Changed Imputed
legend alpha to `1` so the blue square reads at full saturation.

Final alpha pattern: `c(fill_alpha, 1, 1)`.

- Observed: filled at `fill_alpha` (matches the bar fill in the plot).
- Imputed: filled at full opacity (matches the outline opacity in the plot).
- Combined: filled at full opacity (matches the density line opacity).

Each legend entry's alpha now reflects the visual weight of its
corresponding element in the plot, while all three remain filled
squares.

### `distribution_plot` — legend uniform filled squares (2026-05-04, eighth pass)

The Imputed legend entry was rendered as an empty bordered square
(`shape = 0`), which read as visually inconsistent with the filled
Observed and Combined entries. Changed Imputed to a filled square
(`shape = 15`) with the imputed color as fill at `fill_alpha`,
matching the visual weight of the Observed entry. All three legend
entries now render as filled squares, distinguished by color:

- Observed: olive fill at `fill_alpha`
- Imputed: blue fill at `fill_alpha`
- Combined: coral fill at full opacity

The legend communicates the color-to-element mapping; the connection
between an entry and its corresponding plot element is by color match,
not by fill-vs-outline style.

### `distribution_plot` — legend simplified to three squares (2026-05-04, seventh pass)

The dual-dummy line approach for the Combined entry didn't render
reliably — `geom_path`'s key glyph (`draw_key_path`) doesn't honor the
fill scale's override.aes, and the resulting legend showed tiny dots
or no glyph at all for the Combined entry. Reverted to a single
`geom_point` dummy with all three entries as squares, distinguished by
fill/outline only:

- Observed: filled square (`shape = 15`) with `fill = observed_color`
  at `fill_alpha`.
- Imputed: empty bordered square (`shape = 0`) with `color = imputed_color`
  and `linewidth = 1.0`.
- Combined: filled square (`shape = 15`) with `fill = density_color` at
  full opacity. The fill matches the density curve color in the plot, so
  the connection between the legend entry and the curve is visually
  preserved without needing to render an actual line in the legend.

Legend font also bumped:
- `legend.text` and `legend.title` now `font_size + 2` (16 by default,
  matching the title prominence).
- `legend.key.size`, `legend.key.height`, `legend.key.width`, and
  `legend.spacing.x` set explicitly to ensure the keys are visibly
  sized regardless of ggplot version defaults.

### `distribution_plot` — legend rendering, font, title (2026-05-04, sixth pass)

Three issues from rendered output:

1. *Combined entry not rendering as a line in the legend.* The previous
   approach used `geom_point` with `shape = NA` + `linetype = "solid"` +
   `linewidth = 1.0`, expecting `draw_key_point` to render a line for the
   Combined entry. It doesn't — `draw_key_point` ignores linetype and
   linewidth. Fix: use a *second* dummy geom (`geom_path`) for the
   Combined entry. `geom_path`'s default key glyph is `draw_key_path`,
   which renders a horizontal line. `.mixed_legend` now creates two
   dummy geoms (one `geom_point` for Observed/Imputed, one `geom_path`
   for Combined) that both map to the same factor, so ggplot merges
   them into a single three-entry legend with mixed glyphs.

2. *Legend font too small.* The theme inherited from
   `theme_minimal(base_size = font_size)` rendered legend text at
   ggplot's internal default proportion (about `0.8 * base_size`).
   Added explicit `legend.text` and `legend.title` settings at
   `font_size`, plus `legend.key.size = unit(1.2, "lines")` for
   slightly larger key glyphs.

3. *Subtitle moved into title.* The `"X.X% imputed"` subtitle was
   redundant alongside the title. It now appears as a parenthetical at
   the end of the mixed-case title (`"... 20 Imputations: dpdd
   (12.1% imputed)"`). Subtitle is set to `NULL` for all variable
   kinds. "Imputed" is left lowercase in the parenthetical (matches
   convention for short descriptors).

### `distribution_plot` — three-entry legend (2026-05-04, fifth pass)

The mixed-case legend was missing an entry for the density curve.
Added a third entry "Combined" rendered as a line in the legend
(`density_color`, `linewidth = 1.0`, `linetype = "solid"`,
`shape = NA`).

`.mixed_legend` now takes `density_color` as a fourth argument; the
factor used to drive the legend has three levels (Observed, Imputed,
Combined); `override.aes` carries vectors of length 3 for each
attribute. Observed and Imputed entries render as filled square (15)
and empty square (0) respectively; Combined renders as a horizontal
line via the `shape = NA` + `linetype + linewidth` combination.

### `distribution_plot` — weighted density estimation (2026-05-04, fourth pass)

The density curve was tracking the imputed distribution rather than the
post-MI marginal. The cause was unweighted KDE on the stacked observed
+ imputed values: with M = 20 imputations, each missing case appeared 20
times in the stacked data while each observed case appeared once, so
imputed values had M-fold influence on the density estimate.

Fix: weighted density estimation. Observed values get weight 1; imputed
values get weight 1/M. This makes each *case* contribute equally to the
density regardless of how many times it appears in the stacked data, and
yields the post-MI marginal distribution that the curve is meant to
represent.

The weighting is applied in two places:

- `.density_layer` builds a weighted data frame and passes
  `aes(weight = weight)` to `geom_density()`.
- `.distribution_plot_y_upper` uses `stats::density(..., weights = w)`
  with the same weighting scheme to compute the density peak for
  Y-axis clipping.

For model-implied variables (`.residual`, `.latent`, `.probability`),
all values live in `imputed` and each gets weight 1/M, which produces
the average posterior density across cases — the natural interpretation
for a posterior-derived quantity.

`.density_layer` and `.distribution_plot_y_upper` now take `observed`,
`imputed`, and `n_imps` separately rather than a single combined
vector. Internal API change only; no impact on the user-facing
function signature.

### `distribution_plot` — color and alpha tuning (2026-05-04, third pass)

After viewing rendered plots and trying several color/alpha combinations,
the defaults were retuned. The new palette assignment swaps which roles
get which colors from the rblimp 5-color palette:

- `observed_color` is now `"#A3A500"` (olive); was blue.
- `imputed_color` is now `"#00B0F6"` (blue); was coral.
- `density_color` is now `"#F8766D"` (coral); was mint.
- `fill_alpha` is now `0.3`; was 0.90.

The signature now carries an inline comment listing the available
palette codes for quick reference at the call site:

```r
# coral #F8766D olive #A3A500 mint #00BF7D blue #00B0F6 megenta #E76BF3
```

The density-line linewidth was also increased from `0.8` to `1.0`,
matching the histogram outline weight so the visual elements read at
uniform thickness.

### `distribution_plot` — visual fixes (2026-05-04, second pass)

After running the function on a fitted model, five visual issues surfaced
and were fixed:

1. *Title for model-implied variables.* Changed from
   `"Posterior Distribution Across N Iterations: <var>"` to
   `"Distribution of <var>"`, matching the observed-only title format.
   The kind is signaled by the visual style (filled vs. outlined) rather
   than by the title.
2. *Y-axis squashing for continuous variables.* The manual legend's
   dummy points were anchored at `y = 1`, which inflated the Y range and
   squashed the histogram bars into the bottom of the plot. Fixed by
   moving the dummy points to `y = 0` (within the histogram density
   range) and adding `coord_cartesian(ylim = c(0, y_upper))`, where
   `y_upper` is computed from the maximum density across all visible
   layers (histogram bars and, if drawn, density curve) plus 10%
   padding. New helper `.distribution_plot_y_upper` does the
   computation.
3. *Density line on highly discrete variables.* KDE on binary and
   small-ordinal data produces bimodal-with-valley curves that are
   artifacts of bandwidth selection rather than real distribution
   features. The density layer is now suppressed when the variable
   appears in `.get_discrete_vars` (i.e., declared as ORDINAL,
   NOMINAL, or COUNT in the model script). The same `force_integer`
   flag that drives integer X-axis ticks now also gates the density
   layer.
4. *Default `fill_alpha` raised from `0.25` to `0.90`.* The pale blue
   produced by alpha 0.25 against the rblimp blue (`#00B0F6`) read as
   washed out; alpha 0.40 also still felt soft. Setting alpha to 0.90
   keeps the color near its full saturation, with just enough
   transparency that the bars don't read as harsh solid blocks at
   print scale. The palette itself (rblimp blue/coral/mint) is
   unchanged.

The five-fix changelog reflects only post-render adjustments to the
behavior; the underlying structure and API (described below) remain
unchanged.

### `distribution_plot` — completed (2026-05-04, supersedes `imputation_plot` and `univariate_plot`)

**What this function does**

Generates marginal-distribution plots for variables in an rblimp model.
Three variable kinds are classified automatically and rendered with
distinct visual treatments:

- *Observed*: raw variable, no missing values. Filled blue histogram +
  mint density line.
- *Mixed*: raw variable with missing values. Filled blue histogram
  (observed) + outlined coral histogram (imputed) + mint density line
  over the combined post-MI distribution. Subtitle reports
  `"X.X% imputed"`.
- *Model-implied*: variables with suffix `.residual`, `.latent`, or
  `.probability`. Outlined coral histogram + mint density line.

`.predicted` variables remain excluded by default (not useful as
univariate distributions).

All three variable kinds use a single rendering path: histogram with
binwidth determined by integer detection (binwidth = 1 for
integer-valued data, otherwise `diff(range) / bins`) plus a smoothed
density curve. Variables declared as ORDINAL, NOMINAL, or COUNT
additionally receive integer-only X-axis ticks.

**Argument list**

```r
distribution_plot(
  model,
  vars           = NULL,
  bins           = 30,
  font_size      = 14,
  observed_color = "#00B0F6",
  imputed_color  = "#F8766D",
  density_color  = "#00BF7D",
  fill_alpha     = 0.25,
  verbose        = FALSE
)
```

**Behavior changes from the original `imputation_plot`**

- Function renamed to `distribution_plot` because it now also handles
  fully-observed variables and model-implied (`.residual`, `.latent`,
  `.probability`) variables, not only imputation diagnostics.
- Title text varies by variable kind:
  - Observed: `Distribution of <var>`
  - Mixed: `Observed vs. Imputed Distributions Across N Imputations: <var>`
  - Model-implied: `Posterior Distribution Across N Iterations: <var>`
- Subtitle `"X.X% imputed"` reports the percent of cases imputed for
  Mixed variables only (Observed and Model-implied have no subtitle).
- Y-axis ticks and tick labels are removed; the axis label
  (`"Density"`) is retained.
- Density-normalized bars are preserved (required for the shape
  comparison to work); volume context is communicated via the subtitle
  rather than via raw counts.
- `.residual`, `.latent`, and `.probability` are no longer auto-excluded;
  they are now classified as model-implied and rendered with the
  outlined-bar visual treatment.
- `.predicted` remains excluded by default but can be requested explicitly
  via `vars`.
- Categorical (geom_bar) rendering is removed entirely. ORDINAL,
  NOMINAL, COUNT, and any integer-valued continuous variable all go
  through the histogram path with binwidth = 1, producing visually
  equivalent output via a unified rendering path.
- Returned list has class `c("distribution_plot", "list")` with an S3
  print method for auto-display.

**Behavior changes from the original `univariate_plot`**

- Cluster-aware logic from `univariate_plot` (level-2 variable detection,
  random intercept/slope handling for cluster-indexed names like
  `posaff[person]` and `posaff$pain[person]`) is **not** carried over.
  Bracket-name normalization is preserved through `.add_bracket_copies`,
  but the level-2 plotting branch (which extracted unique values per
  cluster for level-2 variables) is gone. If multilevel diagnostic
  plotting is needed in later book chapters, `distribution_plot` will
  need a focused extension for cluster-aware rendering at that point.
- The `stats = TRUE` argument from `univariate_plot` (which printed
  pooled statistics with the plot) is gone. Users who need pooled
  statistics can compute them separately.

**Argument list changes (relative to either function's prior signature)**

- Added: `density_color`, `fill_alpha`.
- Removed: `discrete_vars` (not needed; auto-detection from script
  declarations is comprehensive).
- Removed: `print` (function always returns a list with auto-print
  S3 method).
- Removed: `stats` (was specific to `univariate_plot`).
- Argument order: `model` is now first.

**Structural changes**

- Decomposed into a 60-line user-facing function plus internal helpers:
  `.resolve_plot_vars`, `.classify_var`, `.distribution_plot_extract`,
  `.distribution_plot_one`, `.distribution_plot_data`, `.bar_layers`,
  `.density_layer`, `.distribution_plot_theme`, `.mixed_legend`,
  `.distribution_plot_binwidth`, `.distribution_plot_boundary`,
  `.is_integer_var`, `.integer_breaks`.
- Single rendering path; `kind` controls layering, not rendering style.
- Auto-bin-detection sentinel logic removed; `bins` is now a clean
  argument with default 30.
- Three rblimp palette colors (blue, coral, mint) replace the old
  package globals.

**Behavior changes worth flagging for the programmer**

- Function is silent by default (was verbose in the original). Users
  who relied on the "Plotting variables: ..." messages must pass
  `verbose = TRUE`.
- Color customization no longer accepts the named-palette shorthand
  (`observed_color = "blue"` works as an R color name; the old behavior
  of looking up "blue" in a custom 8-color palette is gone — users
  pass hex codes directly).

### `.get_discrete_vars` — completed (2026-05-04, renamed from `.get_categorical_vars`)

**What changed**

- Renamed from `.get_categorical_vars` to `.get_discrete_vars` (more
  accurate; "categorical" doesn't capture COUNT).
- Extended detection to include COUNT alongside ORDINAL and NOMINAL.
  This is Blimp's complete list of discrete-variable type declarations.
- Range-expansion helper (`expand_range`) consolidated into a single
  function used by both modern (list) and legacy (flat-text) detection
  paths.

**Used by**

- `distribution_plot` for forcing integer-only X-axis ticks on declared
  discrete variables.
- Reserved as shared infrastructure for future plot functions
  (`residuals_plot`, `bivariate_plot`, etc.) that may need to know which
  variables Blimp treats as discrete.

### Remaining functions (in suggested cleanup order)

1. `winsor_mad` — small helper; sets the documentation pattern for
   helper functions.
2. `blimp_theme` — currently used by no one in the cleaned file
   (`distribution_plot` has its own `.distribution_plot_theme`).
   Decide whether to keep, remove, or repurpose as a cross-function
   shared theme.
3. `map_predictor_to_col` and the cluster/level-2 helpers
   (lines 242–601 of original `functions.R`).
4. `standardize_residuals`.
5. `residuals_plot` (longest function — ~1,500 lines; will likely
   decompose into 5–8 internal helpers).
6. `bivariate_plot`.
7. `chibar_test` plus its S3 methods (`print`, `plot`, `summary`).

`univariate_plot` is removed (superseded by `distribution_plot`); see
the function-specific changelog entry above for a list of features
that did not carry over.

## Open questions

- Should `blimp_theme` (in the original file) remain as a shared
  cross-function theme, or be removed in favor of per-plot theme
  helpers like `.distribution_plot_theme`? Decision will be made when
  cleaning the next plot function — if the next function's theme is
  substantially identical to `.distribution_plot_theme`, factor it
  back out into a shared helper.
- Multilevel cluster-aware plotting (level-2 variables, random
  intercept/slope rendering) was dropped from `distribution_plot`.
  This may need to be reintroduced when the multilevel chapter is
  written. Approach: add a `cluster_id` argument to
  `distribution_plot` that triggers a cluster-aware code path for
  level-2 variables, rather than separating it into another function.

## Helpers from the original file that `distribution_plot` depends on

These helpers live in the original `functions.R` and have not yet been
cleaned. They are called by `distribution_plot` and need to be carried
along when the function is integrated into rblimp:

- `.add_bracket_copies` — normalizes cluster-indexed variable names.
- `.normalize_bracket_names` — supporting helper for `.add_bracket_copies`.
- `.extract_model_vars` — extracts variable names from the MODEL block.
- `.syntax_as_text` — converts model syntax (list or `blimp_syntax`) to text.
- `.get_model_block` — returns the MODEL section of the syntax.

These should be cleaned in a future pass and may need their own roxygen
documentation when they're integrated into the rblimp package.
