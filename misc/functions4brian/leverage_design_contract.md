# Contract: the leverage/Cook's design matrix

This documents how `residuals_plot()` builds the design matrix for leverage and
Cook's distance, and why it is correct. It is the one place the code re-derives a
statistic from the predictors instead of reading a saved column, so it is the one
place that has to match the model Blimp actually fit. The rules below are verified
against real Blimp output (see "Validated against").

---

## What this is about (plain terms)

Leverage and Cook's distance are defined by the *exact* set of predictors the
model was fit with. They are computed from the design matrix X: leverage is the
diagonal of X(XᵀX)⁻¹Xᵀ, and Cook's D uses that plus the parameter count. If the
rebuilt X is missing a column the real model had — an interaction, a transformed
term — the numbers come out wrong and still plot, with no error. That was the
original bug: interactions were stripped from X.

The fix does **not** re-implement Blimp's model parser. It takes the list of
fitted terms from the one place that already knows it exactly — the estimates
table — and rebuilds each column's values from saved columns using ordinary
arithmetic and a small whitelist of math functions, with Blimp's own grand means
for centering. Every rebuild is then checked against Blimp's saved fitted values;
where a column can't be rebuilt or the check fails, it draws nothing rather than
something wrong.

These diagnostics are produced only for **single-level continuous and ordinal**
outcomes (multilevel DVs skip OLS influence; nominal/count outcomes use
binned/Pearson residuals and build no design matrix). Nothing else in
`residuals_plot()` / `distribution_plot()` rebuilds a design — every other panel
reads Blimp's saved `.residual` / `.predicted` / `.probability` columns, which
already embed the full fitted model. So this contract governs leverage/Cook's
only; panel selection is untouched and stays as it is.

---

## 1. Where the design comes from: the estimates table

`.model_design_terms(model, dv)` reads `rownames(model@estimates)`. For DV `dv`,
every coefficient row `"<dv> ~ <term>"` names one design column, already expanded
by Blimp into the exact terms it estimated. It keeps those rows, drops the
`(standardized)` / residual-variance / SD / R2 rows and the `Intercept` row (the
builder adds the intercept), and returns the ordered term labels.

Example — `read9 ~ read1 lproblev read1*lproblev esl` (lproblev nominal, 3 cats):

```
read9 ~ Intercept     (dropped; builder adds it)
read9 ~ read1
read9 ~ lproblev.1          }  nominal expanded into its dummies
read9 ~ lproblev.2          }
read9 ~ esl
read9 ~ read1*lproblev.1    }  interaction expanded into products with each dummy
read9 ~ read1*lproblev.2    }
```

→ design terms: `read1`, `lproblev.1`, `lproblev.2`, `esl`,
`read1*lproblev.1`, `read1*lproblev.2`. No model-string parsing; the predictor
equations for other variables (`esl ~ ...`, `read1 ~ ...`) are ignored because
they are not `read9 ~` rows.

The match is by the outcome's **base** variable, not an exact string, so a
**transformed outcome** is handled whichever way Blimp labels its coefficient
rows: each row's left-hand side is reduced to its base variable (stripping a
transform wrapper the same way the DV is detected from the saved `<dv>.*`
columns), so `yjt(dpdd - 6) ~ inflam` and `dpdd ~ inflam` both match `dv = dpdd`.
That keeps outcome transforms (`yjt()`, `ln()`, `sqrt()` on the DV — common in the
book) from losing their influence panels. Note this is about a transform on the
*outcome*; a transform on a *predictor* is handled in §2.

## 2. How each column's values are sourced

`.resolve_design_term(term, data, nominal_vars, center_map)` rebuilds one term's
model-scale values by evaluating it as an expression over the saved columns.
Arithmetic (`+ - * ^`, parentheses) and a **whitelist of Blimp's single-argument,
parameter-free functions** are supported, each mapped to its exact R equivalent:
`abs sqrt exp expm1 log/ln log1p log10 log2 sin cos tan asin acos atan`,
`phi`(→`pnorm`) `iphi`/`probit`(→`qnorm`) `logit`(→`qlogis`) `sigm`(→`plogis`),
and `center`/`stand`/`scale`. Names resolve via `.resolve_atom`. Predictors enter
as their **observed/imputed values**
— verified, not assumed; the saved `.latent` propensity columns are NOT the
regressors:

- **continuous / ordinal / binary predictor** → its own saved column (observed value)
- **nominal-category term `<v>.<k>`** (with `<v>` in `nominal_vars`) → the observed
  dummy `1(round(<v>) == k)`. Building one dummy per listed `k` makes the omitted
  category the reference automatically; no reference level has to be known.
- **interaction `a*b`** (n-way, and grouped like `m*(x1+x2)`) → product of the factors
- **polynomial `x^k`** → the base column raised to the k-th power
- **inline sum score `x1+x2+...+x7`** (how Blimp expands an un-named `x1:+:x7`) →
  elementwise sum of the columns
- **transformed predictor `ln(x+100)`, `logit(p)`, `sqrt(x)`, …** → the whitelisted
  function applied to the (centered, see below) argument
- **named composite or latent factor** → its saved column (observed if present,
  else `<name>.latent`)
- **unknown/non-whitelisted function, or an unresolvable name** → `NULL`:
  un-sourceable, which makes the whole design non-reconstructable

What is deliberately **not** reconstructed (all refuse → absent, never wrong),
checked against Blimp's full `function-reference.md`: `yjt`/`iyjt` (Yeo-Johnson —
depends on an estimated λ not yet read from the estimates; the only commonly-used
one in this bucket); the two-argument group forms `center(x, id)` / `stand(x, id)`
and `min(x, y)` / `max(x, y)` (grouped centering is multilevel, which never reaches
leverage anyway); scalar summaries (`mean`/`sd`/`median` as a lone term are
collinear with the intercept); and recoding / RNG / constant helpers
(`ifelse` `switch` `missing` `bernoulli` `normal` `uniform` `vec` `_pi` …), which
are not predictors — if one is used to build a variable via `TRANSFORM`, that
variable is a saved column and resolves by name. Extending the whitelist to a new
parameter-free function is one line, and the colspace guard keeps it honest.

**Centering.** Blimp grand-mean centers the variables on its `center` line, and
`.center_constants(model)` reads each one's constant from the `"<var> Grand Mean"`
estimate. `.resolve_atom` subtracts it. For a linear term this is a no-op on the
column space (the intercept absorbs it); for a term INSIDE a nonlinear function it
is required, because `ln(x - c + 100)` is not `ln(x + 100)`. A whitelisted
function is added only because its result is **verified** against the saved fitted
values (§3): if a function's centering or form is not reproduced, the colspace
guard refuses rather than mislead. The whitelist is easy to extend — a new Blimp
function is one entry, and the guard keeps it honest.

`.build_design_matrix(data, terms, nominal_vars, center_map)` assembles intercept +
one column per term, or returns `NULL` the moment any term is un-sourceable.

## 3. The column-space guard (the self-check)

Before using a rebuilt X, `.residuals_lm_diagnostics` verifies it reproduces
Blimp's saved fitted values: the saved `<dv>.predicted` must lie in the column
space of X (it is, by construction, a linear combination of the true design's
columns). Concretely, regress `<dv>.predicted` on X per imputation and require
`max|residual|` below `5e-3 * sd(predicted)`; if it fails, a column is missing or
mis-sourced and the function returns `NULL`. (Linear/product/power/sum terms
reproduce to ~1e-5 relative; a correctly-centered nonlinear function to ~1e-4,
since Blimp's per-draw grand mean differs slightly from the pooled one — well
inside the bar; a wrong rebuild, e.g. an un-centered `ln`, lands at ~1e-2 and is
refused. The bar sits between.)

Two points matter here:

- **Not** "Xβ reproduces `.predicted`" using the estimates' coefficients. The
  pooled posterior-mean β does NOT reproduce a single imputation's `.predicted`
  (that uses the per-draw coefficients), so an `Xβ` check fails even on a correct
  design. The gate is "is `.predicted` in the *span* of X", which is coefficient-
  free and returns ~0 residual on the correct design.
- The gate is on `max|residual|`, not a rounded R². A near-miss (e.g. a nonlinear
  term rebuilt with the wrong centering) can be ~0.9999 collinear with the other
  columns, so a loose "R² ≈ 1" would wrongly accept it; the absolute-residual bar
  separates a correct rebuild (~1e-4 relative) from a wrong one (~1e-2). The
  structural rules in §2 are the primary guard (an unknown function or
  unresolvable name refuses before any numeric check); this residual gate is the
  backstop for sourcing mistakes such as observed-vs-latent or a mis-centered
  function.

Net outcome across every model shape: leverage/Cook's are **correct** or
**absent**, never silently wrong.

## 4. Behavior by model shape

For a single-level continuous/ordinal DV:

| Model line | design is… | Leverage/Cook's |
|---|---|---|
| `y ~ a b c` (plain) | all columns sourced | computed |
| `y ~ a b a*b` (interaction) | product rebuilt | computed |
| `y ~ x x^2` (inline power) | power rebuilt (no saved column needed) | computed |
| `y ~ d` where `d` is multi-cat nominal | observed dummies | computed |
| `yscale ~ (a:+:b) m m*(a:+:b)` (**un-named** inline sum + grouped interaction) | sum/product rebuilt from saved columns | computed |
| `yscale ~ xscale ...` where `xscale = a:+:b` is a **named** sum score | composite is a saved column | computed |
| `y ~ ln(x1+100) x2 d` (transform of a centered predictor) | `ln` applied to centered column | computed |
| `y ~ someunknownfn(x)` (function not in the whitelist) | un-sourceable | **absent** (refused) |

## 5. Validated against

The rules in §2–§3 were confirmed by rebuilding X and checking `.predicted ∈
colspace(X)` on real Blimp output (`max|resid| ≈ 5e-5`, saved precision) for:

- **nominal dummy × continuous** interaction (`read9 ~ read1 lproblev read1*lproblev esl`)
- **named sum-score × ordinal** interaction (`yscale ~ xscale m m*xscale`, `xscale = x1:+:x7`)
- **un-named inline sum × ordinal** interaction (`yscale ~ (x1:+:x7) m m*(x1:+:x7)`)
- **inline polynomial** with no saved column (`y ~ x1 x1^2 x2 d1 d2`)
- **inline `ln()` of a centered predictor** (`y ~ ln(x1+100) x2 d`) → **computed**:
  with grand-mean centering the `ln` column reproduces `.predicted` to ~4e-4; the
  un-centered `ln` lands at ~2.6e-2 and is refused, so getting the centering wrong
  can never pass

The arithmetic/function cases reproduce `.predicted` to ~5e-5 (linear) or ~4e-4
(centered nonlinear); the observed-value representation reproduced it and the
latent representation did not; dropping an interaction/power/sum dropped the
reconstruction (R² ≈ 0.66–0.95), which the guard refuses.

## 6. What changed in the code

- `.model_design_terms(model, dv)` — new: design terms from `@estimates`.
- `.center_constants(model)` — new: `var -> grand mean` for Blimp-centered
  variables, from the `"<var> Grand Mean"` estimate rows.
- `.resolve_atom(nm, data, nominal_vars, center_map)` — new: one variable name →
  observed column / nominal dummy / latent factor score (grand-mean centered when
  listed), else `NULL`.
- `.resolve_design_term(term, data, nominal_vars, center_map)` — new: expression
  evaluator over `.resolve_atom` — arithmetic (`+ - * ^`, parentheses) and a
  whitelist of math functions (`ln log log10 log2 exp sqrt abs`); an unknown
  function or unresolvable name → `NULL`.
- `.build_design_matrix(data, terms, nominal_vars, center_map)` — rewritten: builds
  from the `@estimates` term list; `NULL` if any term is un-sourceable. (Was:
  parsed the model string, silently dropped interactions.)
- `.residuals_lm_diagnostics(imp_list, dv, terms, nominal_vars, verbose, center_map)`
  — signature now takes the term list and centering map; added the column-space
  guard; leverage/Cook's math unchanged (pooled-sigma standardizer kept).
- call site in `residuals_plot()` passes `.model_design_terms(model, dv)` and
  `.center_constants(model)`.

Panel selection (`.residuals_plot_predictors` and every residual/probability/
index/level-2 panel), the pooled-sigma standardizer, the single-level-only gate,
and the nominal dummy rule are unchanged.
