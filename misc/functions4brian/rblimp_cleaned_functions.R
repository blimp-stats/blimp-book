# rblimp diagnostic plots ======================================================
#
# Two exported functions produce multiple-imputation diagnostic graphics for a
# fitted rblimp model. Each returns a named list of ggplot objects (S3 class
# "distribution_plot" / "residuals_plot", each with a print method):
#
#   distribution_plot(model)  observed-vs-imputed histograms/densities plus a
#                             normal Q-Q companion, for every plottable variable
#                             (including one-value-per-cluster level-2 quantities).
#   residuals_plot(model)     residual diagnostics dispatched by outcome type:
#                             continuous/ordinal -> linear-style (residual vs
#                             predicted and vs predictor, standardized index,
#                             leverage, Cook's D); nominal -> probability-scale
#                             binned residuals (Gelman & Hill 2007) plus
#                             predicted-probability overlays; count -> Pearson
#                             residuals; plus level-2 diagnostics for a DV's
#                             random effects.
#
# Everything else is an internal helper (`.`-prefixed, marked @keywords
# internal). The file needs only ggplot2 and stats; ggrepel is optional (outlier
# labels when installed, guarded by requireNamespace).
#
# =============================================================================
# POOLING CONTRACT  (load-bearing -- correctness decisions, not style)
# =============================================================================
# Every diagnostic is computed WITHIN each imputed data set and then pooled.
# Nothing is computed on an average of the imputations first: averaging the
# residual and the imputed predictor together induces a spurious residual-
# predictor association. Pooling takes exactly one of three forms:
#
#   (1) STACK all M draws              point clouds -- residual vs predicted /
#       predictor, the standardized index, within-cluster spread, the predicted-
#       probability overlays, and the case-level cloud under each binned panel.
#       Overplotting is tamed by `.pooled_alpha` (alpha scaled by M^(-1/3)),
#       never by thinning draws.
#   (2) FIT-THEN-POOL                  smoothers and binned summaries -- fit or
#       aggregate within each imputation, then average the M results (loess via
#       `.pooled_loess_xy`; binned bin-means averaged across M). Loess is drawn
#       with NO standard-error band.
#   (3) COMPUTE-THEN-AVERAGE-PER-CASE  leverage and Cook's D -- the full statistic
#       is formed inside each imputation, then averaged to one value per case.
#
# STANDARDIZER: all standardizing uses ONE pooled sigma from `.pooled_sigma`
# (the mean of the per-imputation residual SDs, or the per-observation model-
# implied SD when a variance model is fit). The SAME sigma standardizes the
# residual index AND Cook's D. NO between-imputation (Rubin) variance term enters
# ANY point estimate here -- that term is for standard errors, and these plots
# draw none.
#
# =============================================================================
# DELIBERATELY NOT PLOTTED  (each omission is a decision -- do not add back)
# =============================================================================
#   * Normal Q-Q for a `.probability` column (bounded in [0, 1]; an S-curve by
#     construction) or for a CATEGORICAL outcome's `.residual` (a latent-response
#     residual: uninterpretable for nominal/count, normal-by-construction for
#     ordinal). Histograms of these are still drawn.
#   * Gelman & Hill +/-2 SE bands on binned residuals: within-cluster correlation
#     makes them anticonservative, so only bin means and a loess are shown.
#   * Leverage / Cook's D for ANY clustered DV (whenever a clusterid is present):
#     OLS influence ignores the random-effects structure. Single-level DVs only.
#   * Within-cluster residual-variance spread for a non-continuous outcome
#     (Bernoulli / ordinal / count have variance pinned to the mean).
#   * A standardized residual index for a nominal outcome (no meaningful analogue).
#   * A linear-style residual-vs-predicted panel for a count outcome (the latent
#     residual is skewed; Pearson residuals are used instead).
#
# =============================================================================
# PARAMETERS: two kinds
# =============================================================================
# User-facing arguments (colors, sizes, fonts, alpha, bins, qq, index_thresholds,
# max_labels, few_values_warn, discrete_max_unique, vars, verbose) carry defaults
# documented in each function's roxygen block and are meant to be tuned. A second
# set of values is FIXED BY DESIGN and intentionally NOT exposed as arguments:
# the loess span (0.75); floor(sqrt(n)) bins; the <=10-distinct-x switch from a
# loess to category means; the index and within-cluster reference lines; the
# seven-value shape/linetype palettes; the `.pooled_alpha` M^(-1/3) rule. These
# are part of the contract, not free choices to re-tune.
#
# -----------------------------------------------------------------------------
# MULTILEVEL / LEVEL-2 NAMING CONVENTIONS  (read before touching the [ ] logic)
# -----------------------------------------------------------------------------
# Blimp saves two kinds of derived quantities, distinguished by how the name is
# decorated:
#
#   LEVEL-1 (one value per row, dot-suffixed):
#     <dv>.residual, <dv>.predicted, <dv>.probability, <dv>.latent, <dv>.yjt
#     var.<dv>.predicted  (per-case log-variance, present only when a variance
#                          model is fit; consumed by .pooled_sigma)
#
#   LEVEL-2 (one value per CLUSTER, repeated down the rows of that cluster,
#            named with the cluster-ID variable in square brackets):
#     <dv>[cluster]        the DV's random-intercept residual (e.g. probsolve[school])
#     <x>.mean[cluster]    a latent group mean of <x> (a level-2 *variable*, and a
#                          level-2 *predictor* only if it is on a DV's MODEL line)
#     <dv>$<x>[cluster]    a random-slope residual (not diagnosed; the `$` guard
#                          in .is_dv_level2_residual skips it)
#
# Key facts the plotting code relies on:
#   * The bracket holds the cluster-ID column name, and it exists BECAUSE a
#     3-level model can have two cluster IDs; the bracket disambiguates which
#     one. (These examples are 2-level, so there is one.)
#   * The level-2 residual is the BARE bracketed column. There is no
#     `<dv>[cluster].residual` / `.predicted`; the dot-suffixed names are all
#     LEVEL-1. So "is <name> the DV's level-2 residual?" is decided by testing
#     whether its base (before `[`) carries the level-1 `.residual`/`.predicted`
#     columns -- not by any suffix on the bracketed name. See
#     `.is_dv_level2_residual`.
#   * Everything ending in `[cluster]` is a level-2 *variable* and is deduplicated
#     to one-value-per-cluster before plotting (distribution_plot). But only the
#     DV's `[cluster]` residual gets residual diagnostics, and only MODEL-line
#     predictors that are constant within cluster count as level-2 *predictors*
#     (residuals_plot). See `.is_level2_var`, `.cluster_id_of`,
#     `.level2_predictor_cols`.
# -----------------------------------------------------------------------------

# ggplot2 maps aesthetics to bare column names through non-standard evaluation,
# so R CMD check reports those names as undefined global variables. Declaring
# them here (in a package, this can live in any R/ file) silences that NOTE
# without changing behavior.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "x", "y", "z", "prob", "category", "case", "cluster", "leverage", "cooks",
    "theoretical", "x_mean", "resid_mean", "pred_mean", "value", "type", "weight"
  ))
}



# .get_discrete_vars ===========================================================
#
# Returns variable names declared as ORDINAL, NOMINAL, or COUNT in the model
# script. Expands ranges like dep1:dep7 -> dep1, dep2, ..., dep7. Works for
# both modern list syntax ($ordinal, $nominal, $count) and legacy flat text
# with ORDINAL:, NOMINAL:, COUNT: headers.
#
# Used by distribution_plot to force integer-only X-axis ticks on declared
# discrete variables, and by residuals_plot to dispatch on outcome type.

#' @keywords internal
.get_discrete_vars <- function(model, types = c("ordinal", "nominal", "count")) {
  types <- match.arg(types, several.ok = TRUE)
  sx    <- model@syntax
  out   <- character(0)

  # Range-expansion helper shared between modern and legacy paths
  expand_range <- function(tok) {
    if (!grepl(":", tok, fixed = TRUE)) return(tok)
    m <- regexec("^([A-Za-z._]+)([0-9]+):\\1([0-9]+)$", tok, perl = TRUE)
    res <- regmatches(tok, m)[[1]]
    if (length(res) != 4L) return(tok)
    base  <- res[2]
    start <- as.integer(res[3])
    end   <- as.integer(res[4])
    if (!is.finite(start) || !is.finite(end)) return(tok)
    paste0(base, seq.int(start, end))
  }

  # Modern blimp_syntax / list case ----
  if ((inherits(sx, "blimp_syntax") || is.list(sx)) &&
      any(types %in% names(sx))) {

    pieces <- unlist(
      lapply(types, function(t) if (t %in% names(sx)) sx[[t]] else NULL),
      use.names = FALSE
    )
    pieces <- pieces[!is.na(pieces)]

    if (length(pieces)) {
      tokens <- unlist(
        strsplit(paste(pieces, collapse = " "), "\\s+"),
        use.names = FALSE
      )
      tokens <- tokens[nzchar(tokens)]
      out    <- unique(unlist(lapply(tokens, expand_range), use.names = FALSE))
    }
  }

  # Legacy flat-text case (ORDINAL: / NOMINAL: / COUNT: headers) ----
  if (!length(out)) {
    txt <- .syntax_as_text(sx)
    if (!nzchar(txt)) return(character(0))
    txt1 <- gsub("\\s+", " ", txt)

    grab <- function(key) {
      rx <- paste0("(?i)\\b", key, "\\s*:\\s*([^;]+)\\s*;")
      m  <- gregexpr(rx, txt1, perl = TRUE)
      segs <- regmatches(txt1, m)[[1]]
      if (!length(segs)) return(character(0))

      toks <- unlist(lapply(segs, function(seg) {
        s   <- sub(rx, "\\1", seg, perl = TRUE)
        s   <- gsub(",", " ", s)
        tt  <- strsplit(s, "\\s+", perl = TRUE)[[1]]
        tt[nzchar(tt)]
      }), use.names = FALSE)

      unique(toks)
    }

    tokens <- unique(unlist(lapply(toupper(types), grab), use.names = FALSE))
    if (!length(tokens)) return(character(0))

    out <- unique(unlist(lapply(tokens, expand_range), use.names = FALSE))
  }

  out
}



# .syntax_as_text ==============================================================
#
# Coerce model@syntax to a single character blob. Handles modern blimp_syntax
# objects (which are lists), legacy character syntax, and arbitrary S4
# objects via as.character().

#' @keywords internal
.syntax_as_text <- function(sx) {
  if (inherits(sx, "blimp_syntax") || is.list(sx)) {
    paste(unlist(sx, use.names = FALSE), collapse = "\n")
  } else if (is.character(sx)) {
    paste(sx, collapse = "\n")
  } else {
    paste(as.character(sx), collapse = "\n")
  }
}



# .get_model_block =============================================================
#
# Pulls the MODEL: block lines from a model's syntax. Handles both modern
# blimp_syntax/list objects (via $model) and legacy flat-text syntax (via
# the "MODEL:" header). Stops at the next ALL-CAPS section header
# (SEED:, OPTIONS:, SAVE:, etc.) when parsing legacy text.

#' @keywords internal
.get_model_block <- function(model) {
  if (is.null(model@syntax)) return(character(0))
  sx <- model@syntax

  # Modern blimp_syntax / list with a $model component
  if ((inherits(sx, "blimp_syntax") || is.list(sx)) && "model" %in% names(sx)) {
    mb <- as.character(sx$model)
    mb <- mb[!is.na(mb)]
    mb <- trimws(mb)
    mb <- mb[nzchar(mb)]
    return(mb)
  }

  # Legacy flat text with "MODEL:" header
  txt <- .syntax_as_text(sx)
  if (!nzchar(txt)) return(character(0))

  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  if (!length(lines)) return(character(0))

  idx_model <- grep("^\\s*MODEL\\s*:", lines, ignore.case = TRUE)
  if (!length(idx_model)) return(character(0))

  lines2    <- lines[idx_model[1]:length(lines)]
  lines2[1] <- sub("^\\s*MODEL\\s*:\\s*", "", lines2[1], ignore.case = TRUE)

  if (length(lines2) > 1L) {
    hdr_idx <- which(grepl("^\\s*[A-Z]+\\s*:", lines2[-1L]))
    if (length(hdr_idx)) {
      stop_at <- hdr_idx[1L] + 1L
      lines2  <- lines2[seq_len(stop_at)]
    }
  }

  lines2
}



# .extract_model_vars ==========================================================
#
# Extracts all variable names appearing anywhere in the MODEL: block, after
# stripping path labels (e.g., x@b1) and reserved tokens (~, |, intercept).
# Also unions in any variables declared as ORDINAL, NOMINAL, or COUNT.

#' @keywords internal
.extract_model_vars <- function(model) {
  if (is.null(model@syntax))
    stop("@syntax not found on model object")

  sx <- model@syntax

  # Modern objects: list / blimp_syntax with $model
  if ((inherits(sx, "blimp_syntax") || is.list(sx)) && "model" %in% names(sx)) {
    model_text <- as.character(sx$model)
    model_text <- paste(model_text, collapse = " ")
  } else {
    # Legacy text: locate lines starting with MODEL:
    sx_chr <- as.character(sx)
    lines  <- unlist(strsplit(paste(sx_chr, collapse = "\n"), "\n", fixed = TRUE))
    if (!length(lines))
      stop("Could not coerce @syntax to character.")
    model_lines <- grep("^\\s*MODEL\\s*:", lines, value = TRUE, ignore.case = TRUE)
    if (!length(model_lines))
      stop("No MODEL section found in @syntax")
    model_text <- gsub(
      "^\\s*MODEL\\s*:\\s*", "",
      paste(model_lines, collapse = " "), ignore.case = TRUE
    )
  }

  # Strip path labels like depress_sum@b1
  model_text <- gsub("@[A-Za-z0-9_.]+", "", model_text)

  # Tokenize variable names
  clean <- gsub("[^A-Za-z0-9_.]+", " ", model_text)
  toks  <- unlist(strsplit(clean, "\\s+"))
  toks  <- toks[nzchar(toks)]

  reserved   <- c("~", "|", "intercept")
  toks       <- setdiff(toks, reserved)
  model_vars <- unique(toks)

  # Also include declared discrete vars (ordinal / nominal / count)
  cat_vars <- .get_discrete_vars(model)

  unique(c(model_vars, cat_vars))
}



# .normalize_bracket_names =====================================================
#
# Converts bracket-notation variable names to dot notation:
#   posaff[person]              -> posaff.person
#   posaff$pain[person]         -> posaff_on_pain.person
#   pain.mean[person]           -> pain.mean.person
# Used to create plottable copies of bracketed multilevel variables.

#' @keywords internal
.normalize_bracket_names <- function(names_vec) {
  vapply(names_vec, function(nm) {
    if (!grepl("\\[", nm)) return(nm)

    cluster <- sub("^.+\\[(.+)\\]$", "\\1", nm)

    if (grepl("\\$", nm)) {
      # var$slope[cluster] -> var_on_slope.cluster
      base  <- sub("^(.+)\\$.+\\[.+\\]$", "\\1", nm)
      slope <- sub("^.+\\$(.+)\\[.+\\]$", "\\1", nm)
      paste0(base, "_on_", slope, ".", cluster)
    } else {
      # var[cluster] -> var.cluster (preserves dots in base)
      base <- sub("\\[.+\\]$", "", nm)
      paste0(base, ".", cluster)
    }
  }, character(1L), USE.NAMES = FALSE)
}



# Level-2 (bracketed) variable predicates ======================================
#
# Blimp appends the cluster-ID variable in square brackets to every level-2
# quantity it saves: a random-intercept residual `probsolve[school]`, a random
# slope `posaff$pain[person]`, or a latent group mean `stanmath.mean[school]`.
# Any name ending in `[<clusterid>]` is therefore a level-2 quantity, stored
# repeated across the level-1 rows of its cluster.
#
#   .is_level2_var   TRUE for any name ending in `[...]`.
#   .is_level2_mean  TRUE for latent group means (`*.mean[...]`), which are
#                    level-2 predictors rather than residuals.
#   .cluster_id_of   returns the bracketed cluster-ID column name.

#' @keywords internal
.is_level2_var <- function(x) grepl("\\[.+\\]$", x)

#' @keywords internal
.is_level2_mean <- function(x) grepl("\\.mean\\[.+\\]$", x)

#' @keywords internal
.cluster_id_of <- function(x) sub("^.+\\[(.+)\\]$", "\\1", x)



# .add_bracket_copies ==========================================================
#
# For each bracketed variable in @imputations and @variance_imp, adds a
# dot-notation copy under a normalized name (see .normalize_bracket_names).
# Returns the model unchanged when no bracketed variables are present.

#' @keywords internal
.add_bracket_copies <- function(model) {
  var_names    <- names(model@imputations[[1]])
  bracket_vars <- var_names[grepl("\\[", var_names)]

  if (!length(bracket_vars)) return(model)

  normalized_names <- .normalize_bracket_names(bracket_vars)

  # Add copies to each imputation
  for (i in seq_along(model@imputations)) {
    for (j in seq_along(bracket_vars)) {
      original_name <- bracket_vars[j]
      new_name      <- normalized_names[j]
      if (!new_name %in% names(model@imputations[[i]])) {
        model@imputations[[i]][[new_name]] <-
          model@imputations[[i]][[original_name]]
      }
    }
  }

  # Add copies to variance_imp if present
  if (!is.null(model@variance_imp)) {
    for (j in seq_along(bracket_vars)) {
      original_name <- bracket_vars[j]
      new_name      <- normalized_names[j]
      if (original_name %in% names(model@variance_imp) &&
          !new_name %in% names(model@variance_imp)) {
        model@variance_imp[[new_name]] <-
          model@variance_imp[[original_name]]
      }
    }
  }

  model
}



# distribution_plot ============================================================

#' Plot the marginal distribution of variables in an rblimp model
#'
#' Builds a marginal-distribution plot for each variable in an rblimp model.
#' Three variable kinds are supported, classified automatically:
#'
#' \describe{
#'   \item{Observed}{Raw variables with no missing values. Rendered as a
#'     filled, semi-transparent histogram with a smoothed density line.}
#'   \item{Mixed}{Raw variables with missing values. Rendered as filled bars
#'     for observed values, outlined bars for imputed values, and a density
#'     line over the combined post-MI distribution. The subtitle reports the
#'     percent of cases with imputed values.}
#'   \item{Model-implied}{Derived variables with suffix `.residual`,
#'     `.latent`, or `.probability`. Rendered as outlined bars with a
#'     smoothed density line. (`.predicted` is excluded by default.)}
#'   \item{Level-2}{Bracketed cluster-level quantities (`probsolve[school]`,
#'     `stanmath.mean[school]`, random slopes). Deduplicated to one value per
#'     cluster (keyed on the bracketed cluster-ID column) before plotting, then
#'     rendered like a model-implied distribution.}
#' }
#'
#' All three variable kinds use the same rendering path: a histogram with
#' binwidth determined by integer detection (binwidth = 1 for integer-valued
#' data, otherwise `diff(range) / bins`) plus a smoothed density curve.
#' Variables declared as ORDINAL, NOMINAL, or COUNT in the model script
#' additionally receive integer-only X-axis ticks.
#'
#' Returned plots are ggplot2 objects; the returned list has class
#' `"distribution_plot"` with an S3 print method that auto-displays each plot
#' when called interactively.
#'
#' @param model An rblimp model object. Must have non-empty `@imputations`
#'   and `@variance_imp` slots.
#' @param vars Optional character vector of variable names to plot. If `NULL`
#'   (default), plots every variable that appears in the MODEL section and
#'   has imputation data, plus model-implied variables (`.residual`,
#'   `.latent`, `.probability`).
#' @param bins Integer. Target number of histogram bins for continuous
#'   variables. Default `30`. For integer-valued variables, binwidth is
#'   forced to `1` regardless of this argument.
#' @param font_size Numeric. Base font size in points. Default `14`.
#' @param observed_color Color for observed values (filled bars). Default
#'   `"#A3A500"` (olive).
#' @param imputed_color Color for imputed and model-implied values (outlined
#'   bars). Default `"#00B0F6"` (blue).
#' @param density_color Color for the smoothed density curve. Default
#'   `"#F8766D"` (coral).
#' @param fill_alpha Numeric in [0, 1]. Alpha for filled bar layers. Default
#'   `0.3`.
#' @param line_width Numeric. Line weight for the imputed histogram outline
#'   and the density curve. Default `1.0`. For book/print figures with
#'   thin axis lines, values around `0.5` to `0.7` read cleaner.
#' @param qq Logical. If `TRUE` (default), a normal Q-Q companion is added for
#'   every continuous variable, named `"<var>.qq"` (e.g., `"stanmath.qq"`,
#'   `"probsolve.residual.qq"`, `"probsolve[school].qq"`). The Q-Q is the
#'   univariate normality view of the same values shown in the histogram, so it
#'   lives here rather than in `residuals_plot`. It is suppressed for declared
#'   discrete variables (ORDINAL/NOMINAL/COUNT) and for variables that are
#'   *empirically* discrete (see `discrete_max_unique`), where a normal
#'   reference would mislead. Set `FALSE` for histograms only.
#' @param discrete_max_unique Integer. A numeric variable with this many or
#'   fewer distinct values is treated as discrete even when it was not declared
#'   ORDINAL/NOMINAL/COUNT: its density curve and normal Q-Q are suppressed and
#'   its x-axis is forced to integer ticks. This catches undeclared binary
#'   indicators and coarse variables (e.g., `intschool`) that would otherwise
#'   get a nonsensical banded Q-Q. Default `5`.
#' @param verbose Logical. If `TRUE`, prints informational messages about
#'   variables being plotted or skipped. Default `FALSE`.
#'
#' @return A named list of class `"distribution_plot"`, with one ggplot
#'   object per variable (named by the variable) and, when `qq = TRUE`, one
#'   `"<var>.qq"` normal-Q-Q object per continuous variable. Variables that
#'   cannot be plotted (e.g., non-numeric data, no observed or imputed values
#'   present) are silently dropped unless `verbose = TRUE`. Calling the list at
#'   the console auto-prints every plot in turn; assigning the result is silent.
#'
#' @examples
#' \dontrun{
#'   plots <- distribution_plot(my_model)        # auto-prints all plots
#'   plots$dpdd                                  # one plot
#'   distribution_plot(my_model, vars = "dpdd")  # restrict to one variable
#' }
#'
#' @export
distribution_plot <- function(
  model,
  vars           = NULL,
  bins           = 30,
  font_size      = 14,
  observed_color = "#A3A500",
  imputed_color  = "#00B0F6",
  density_color  = "#F8766D",
  fill_alpha     = .3,
  line_width     = 1.0,
  qq             = TRUE,
  discrete_max_unique = 5L,
  verbose        = FALSE
  # coral #F8766D olive #A3A500 mint #00BF7D blue #00B0F6 megenta #E76BF3
) {

  # Input validation
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!isS4(model) || !.hasSlot(model, "imputations")) {
    stop("`model` must be an rblimp model object.")
  }
  if (!is.list(model@imputations) || !length(model@imputations)) {
    stop("`model@imputations` must be a non-empty list.")
  }
  if (is.null(model@variance_imp)) {
    stop("`model@variance_imp` was not found on the model object.")
  }
  if (!is.numeric(fill_alpha) || fill_alpha < 0 || fill_alpha > 1) {
    stop("`fill_alpha` must be a number between 0 and 1.")
  }

  # Setup
  model         <- .add_bracket_copies(model)
  discrete_vars <- .get_discrete_vars(model)
  vars          <- .resolve_plot_vars(model, vars, verbose)
  n_imps        <- length(model@imputations)

  # Common style bundle passed to every plot helper
  style <- list(
    font_size      = font_size,
    observed_color = observed_color,
    imputed_color  = imputed_color,
    density_color  = density_color,
    fill_alpha     = fill_alpha,
    line_width     = line_width
  )

  # Build the histogram/density plot for each variable and, when `qq = TRUE`,
  # a normal Q-Q companion for each *continuous* variable (added as
  # "<var>.qq"). The Q-Q is a univariate distributional view of the same
  # values -- it belongs here beside the histogram, not in residuals_plot -- and
  # gives the normality check for level-1 `.residual`, the level-2 `[cluster]`
  # residual, latent responses, and continuous raw variables. It is suppressed
  # for declared-discrete variables (the same `force_integer` gate that
  # suppresses the density line), where a normal Q-Q would mislead.
  plots <- list()
  for (v in vars) {
    # A variable is treated as discrete if it is DECLARED discrete
    # (ORDINAL/NOMINAL/COUNT) or is EMPIRICALLY discrete (too few distinct
    # values, e.g. an undeclared binary indicator like `intschool`). Discrete
    # treatment suppresses the density curve, forces integer x-axis ticks, and
    # skips the normal Q-Q -- all of which are misleading for few-valued data.
    force_integer <- (v %in% discrete_vars) ||
      .is_effectively_discrete(model@imputations[[1]][[v]], discrete_max_unique)

    hp <- .distribution_plot_one(
      var           = v,
      model         = model,
      n_imps        = n_imps,
      force_integer = force_integer,
      bins          = bins,
      style         = style,
      verbose       = verbose
    )
    if (!is.null(hp)) plots[[v]] <- hp

    if (qq && !force_integer) {
      qp <- .distribution_plot_qq(v, model, style, discrete_max_unique)
      if (!is.null(qp)) plots[[paste0(v, ".qq")]] <- qp
    }
  }

  class(plots) <- c("distribution_plot", "list")
  plots
}



# print method =================================================================

#' @export
print.distribution_plot <- function(x, ...) {
  for (p in x) print(p)
  invisible(x)
}



# Internal: variable resolution ================================================
#
# Determines which variables get plotted. Includes raw model variables and
# the four model-implied suffixes (.residual, .latent, .probability, .yjt).
# Excludes .predicted.

#' @keywords internal
.resolve_plot_vars <- function(model, vars, verbose) {
  vars_imp1   <- names(model@imputations[[1]])
  vars_varimp <- names(model@variance_imp)
  vars_all    <- intersect(vars_varimp, vars_imp1)

  # Excluded by default; .predicted variables aren't useful as univariate
  # distributions. Users can still request them explicitly via `vars`.
  vars_eligible <- vars_all[!grepl("\\.predicted$", vars_all)]

  # Level-2 (bracketed) quantities live in @imputations but are not always
  # present in @variance_imp, so they are picked up from the imputation names
  # directly rather than from `vars_all`. This covers random-effect residuals
  # (`probsolve[school]`), random slopes, and latent group means
  # (`stanmath.mean[school]`) -- every name ending in `[clusterid]`.
  level2_vars <- vars_imp1[.is_level2_var(vars_imp1)]

  if (is.null(vars)) {
    model_vars       <- .extract_model_vars(model)
    model_implied_rx <- "\\.(residual|latent|probability|yjt)$"
    model_implied    <- vars_eligible[grepl(model_implied_rx, vars_eligible)]
    raw_present      <- intersect(vars_eligible, model_vars)
    vars             <- unique(c(raw_present, model_implied, level2_vars))

    if (!length(vars)) {
      stop("No matching variables found in MODEL section with imputation data.")
    }
    if (verbose) {
      message("Plotting variables: ", paste(vars, collapse = ", "))
    }
  } else {
    missing_vars <- setdiff(vars, union(vars_all, level2_vars))
    if (length(missing_vars)) {
      stop("Variable(s) not found: ", paste(missing_vars, collapse = ", "))
    }
  }
  vars
}



# Internal: classify variable kind =============================================
#
# Returns one of: "observed", "mixed", "implied".

#' @keywords internal
.classify_var <- function(var, miss_idx) {
  if (grepl("\\.(residual|latent|probability|yjt)$", var)) {
    return("implied")
  }
  if (any(miss_idx)) "mixed" else "observed"
}



# Internal: extract values for one variable ====================================
#
# Returns a list of components needed downstream, or NULL if the variable
# can't be plotted.

#' @keywords internal
.distribution_plot_extract <- function(var, model) {
  # Level-2 (bracketed) quantities are handled separately: they are stored
  # repeated across the level-1 rows of each cluster and must be collapsed to
  # one value per cluster before plotting (see .distribution_plot_extract_level2).
  if (.is_level2_var(var)) return(.distribution_plot_extract_level2(var, model))

  vimp <- model@variance_imp[[var]]
  if (is.null(vimp)) return(NULL)

  # Blimp convention: nonzero finite entries flag missing/imputed cases.
  # For .residual / .latent / .probability / .yjt the variance_imp entries
  # flag iteration-varying cases, so this captures all values that vary
  # across iterations (i.e., the model-implied posterior draws).
  miss_idx <- is.finite(vimp) & (vimp != 0)

  obs_vals <- model@imputations[[1]][[var]][!miss_idx]
  imp_vals <- unlist(
    lapply(model@imputations, function(d) d[[var]][miss_idx]),
    use.names = FALSE
  )

  obs_vals <- obs_vals[is.finite(obs_vals)]
  imp_vals <- imp_vals[is.finite(imp_vals)]

  kind <- .classify_var(var, miss_idx)

  # For model-implied variables, treat all values as "imputed" since none
  # are observed in the data sense.
  if (kind == "implied") {
    obs_vals <- numeric(0)
    imp_vals <- unlist(
      lapply(model@imputations, function(d) d[[var]]),
      use.names = FALSE
    )
    imp_vals <- imp_vals[is.finite(imp_vals)]
  }

  if (kind == "observed" && !length(obs_vals))     return(NULL)
  if (kind == "mixed"    && (!length(obs_vals) || !length(imp_vals))) return(NULL)
  if (kind == "implied"  && !length(imp_vals))     return(NULL)
  if (length(obs_vals) && !is.numeric(obs_vals))   return(NULL)
  if (length(imp_vals) && !is.numeric(imp_vals))   return(NULL)

  pct_imp <- if (kind == "mixed") 100 * sum(miss_idx) / length(miss_idx) else NA_real_

  list(
    kind     = kind,
    observed = obs_vals,
    imputed  = imp_vals,
    pct_imp  = pct_imp
  )
}



# Internal: extract values for one level-2 variable ============================
#
# Level-2 quantities (names ending in `[clusterid]`) are stored repeated across
# every level-1 row of a cluster. Plotting the raw column would weight each
# cluster by its size and report an inflated n; instead we collapse to one
# value per cluster -- keyed on the bracketed cluster-ID column -- within each
# imputation, then pool the draws across imputations. The result is rendered
# with the model-implied ("implied") path (outlined bars + density line), since
# a level-2 residual has no observed/imputed distinction in the data sense.
# Returns NULL if the values are unusable (non-numeric, empty, or all missing).

#' @keywords internal
.distribution_plot_extract_level2 <- function(var, model) {
  cluster_col <- .cluster_id_of(var)

  vals <- unlist(lapply(model@imputations, function(d) {
    x <- d[[var]]
    if (is.null(x)) return(numeric(0))
    g <- d[[cluster_col]]
    # One row per cluster. If the cluster-ID column is absent we cannot safely
    # deduplicate (distinct clusters may share a value by chance), so fall back
    # to the raw column rather than silently collapsing genuine ties.
    if (!is.null(g)) x <- x[!duplicated(g)]
    x
  }), use.names = FALSE)

  vals <- vals[is.finite(vals)]
  if (!length(vals) || !is.numeric(vals)) return(NULL)

  list(
    kind     = "implied",
    observed = numeric(0),
    imputed  = vals,
    pct_imp  = NA_real_
  )
}



# Internal: normal Q-Q companion for one variable ==============================
#
# A univariate normality check: the variable's values (post-MI, and already
# collapsed to one-per-cluster for level-2 quantities via
# .distribution_plot_extract) are standardized and plotted against theoretical
# normal quantiles, with a 45-degree reference line. Points off the line to the
# upper-right/lower-left indicate heavy tails; a systematic S- or arch-shape
# indicates skew. Only called for continuous variables (the caller applies the
# same `force_integer` gate that suppresses the density line). Returns NULL when
# fewer than three finite values are available or the values have no spread.

#' @keywords internal
.distribution_plot_qq <- function(var, model, style, max_unique = 5L) {
  # Predicted probabilities are bounded in [0, 1] and are not expected to be
  # normal, so a normal Q-Q of them is nonsensical (it bends into an S by
  # construction). The histogram of a `.probability` variable is still fine; only
  # its Q-Q is suppressed.
  if (grepl("\\.probability$", var)) return(NULL)

  # A CATEGORICAL outcome's `.residual` is a latent-response residual, not an
  # observed-scale one: for a NOMINAL DV it is the Polya-Gamma latent residual,
  # for ORDINAL the probit/logit latent residual, for COUNT the latent-scale
  # residual. None is a meaningful target for a normal Q-Q -- the nominal/count
  # latent residual is not interpretable as an approximately-normal quantity,
  # and the ordinal latent residual is normal essentially *by construction*, so
  # its Q-Q is a tautology rather than a check. The Q-Q is therefore suppressed
  # for any nominal/ordinal/count DV's `.residual`; the histogram is still built
  # by the caller. Continuous-DV `.residual` Q-Qs (the real normality check) and
  # the level-2 `[cluster]` random-effect Q-Q are unaffected -- neither ends in
  # `.residual` for a categorical outcome here. Note `.classify_outcome_type`
  # resolves the `<parent>.<k>` nominal naming, so `turnover.1.residual` maps to
  # the nominal parent `turnover`.
  if (grepl("\\.residual$", var)) {
    base  <- sub("\\.residual$", "", var)
    otype <- .classify_outcome_type(
      base,
      ordinal_vars = .get_discrete_vars(model, types = "ordinal"),
      nominal_vars = .get_discrete_vars(model, types = "nominal"),
      count_vars   = .get_discrete_vars(model, types = "count")
    )$type
    if (otype %in% c("nominal", "ordinal", "count")) return(NULL)
  }

  data <- .distribution_plot_extract(var, model)
  if (is.null(data)) return(NULL)

  vals <- c(data$observed, data$imputed)
  vals <- vals[is.finite(vals)]
  if (length(vals) < 3) return(NULL)

  # Defensive backstop: a normal Q-Q of few-valued (binary/small-ordinal) data
  # degenerates into horizontal bands and is misleading. The caller already
  # gates on this, but guard here too so direct calls stay safe.
  if (.is_effectively_discrete(vals, max_unique)) return(NULL)

  s <- stats::sd(vals)
  if (!is.finite(s) || s == 0) return(NULL)
  z <- (vals - mean(vals)) / s

  qq <- stats::qqnorm(z, plot.it = FALSE)
  df <- data.frame(theoretical = qq$x, sample = qq$y)

  # Square the plot: symmetric, equal x/y limits set to the largest absolute
  # quantile on either axis (so the data is always contained -- no clipping),
  # with a 1:1 aspect via coord_fixed. This makes the identity reference line a
  # true 45-degree diagonal, so departures from normality read honestly and
  # proportionally in both directions regardless of the panel's shape.
  #
  # Axis labels: the empirical (sample) quantiles are on the VERTICAL axis, and
  # they are STANDARDIZED, so the y-axis reads "Standardized <var label>" -- the
  # same variable label the histogram uses (.distribution_plot_xlab), prefixed to
  # flag the scale. The horizontal axis is "Standard Normal Quantiles" (the
  # theoretical, also standardized, reference distribution).
  #
  # Axis lines: unlike the histograms -- which sit on a bar baseline and hide the
  # (density) y-axis -- a Q-Q is a floating scatter, so it gets BOTH x and y axis
  # lines plus visible y-axis ticks/text, overriding the shared theme's blanked
  # y-axis, so the plot reads as an anchored scatter rather than floating.
  lim <- max(abs(c(df$theoretical, df$sample)), na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey50",
                         linetype = "dashed",
                         linewidth = style$line_width * 0.5) +
    ggplot2::geom_point(color = style$imputed_color,
                        alpha = 0.5, size = style$line_width) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    ggplot2::labs(
      title = sprintf("Normal Q-Q: %s", .distribution_plot_xlab(var)),
      x     = "Standard Normal Quantiles",
      y     = paste("Standardized", .distribution_plot_xlab(var))
    ) +
    .distribution_plot_theme(style$font_size, show_legend = FALSE) +
    ggplot2::theme(
      axis.line.x  = ggplot2::element_line(color = "black",
                                           linewidth = style$line_width * 0.5),
      axis.line.y  = ggplot2::element_line(color = "black",
                                           linewidth = style$line_width * 0.5),
      axis.text.y  = ggplot2::element_text(size = style$font_size),
      axis.ticks.y = ggplot2::element_line(color = "black",
                                           linewidth = style$line_width * 0.5)
    )
}



# Internal: build one plot =====================================================
#
# Dispatches based on variable kind. Single rendering path; differences are
# in which bar layers are drawn, which title is used, and whether a legend
# is shown.

#' @keywords internal
.distribution_plot_one <- function(var, model, n_imps, force_integer, bins, style, verbose) {
  data <- .distribution_plot_extract(var, model)
  if (is.null(data)) {
    if (verbose) message("Skipping variable with insufficient data: ", var)
    return(NULL)
  }

  # Title varies by kind. The percent-imputed indicator is folded into
  # the mixed-case title as a parenthetical.
  title_var <- .distribution_plot_xlab(var)
  title <- switch(
    data$kind,
    observed = sprintf("Distribution of %s", title_var),
    mixed    = sprintf(
      "Observed vs. Imputed Distributions: %s (%.1f%% imputed)",
      title_var, data$pct_imp
    ),
    implied  = sprintf("Distribution of %s", title_var)
  )

  # Compute binwidth and boundary from whichever values exist
  all_vals <- c(data$observed, data$imputed)
  binwidth <- .distribution_plot_binwidth(all_vals, bins)
  boundary <- .distribution_plot_boundary(all_vals, binwidth)

  # Density layer is suppressed for declared discrete variables (ORDINAL,
  # NOMINAL, COUNT). For binary and small-ordinal data, KDE produces visually
  # misleading bimodal curves; suppressing avoids the artifact entirely.
  show_density <- !force_integer

  # Compute the kernel bandwidth explicitly from the unweighted data.
  # Passing this to density() and geom_density() silences the
  # "Selecting bandwidth *not* using 'weights'" warning that stats::density
  # emits when weights are supplied without an explicit bandwidth.
  bw <- if (length(all_vals) >= 2) stats::bw.nrd0(all_vals) else 1

  # Compute Y-axis upper limit from histogram peaks (and density peak if drawn).
  # Without this, the dummy points used to construct the manual legend can
  # inflate the Y range and squash the histogram bars.
  y_upper <- .distribution_plot_y_upper(data, n_imps, binwidth, boundary, show_density, bw)

  # Build the plot
  df <- .distribution_plot_data(data)
  p  <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    .bar_layers(df, data$kind, binwidth, boundary, style)

  if (show_density) {
    p <- p + .density_layer(data$observed, data$imputed, n_imps, bw, style)
  }

  p <- p +
    ggplot2::labs(title = title, x = .distribution_plot_xlab(var), y = "Density") +
    ggplot2::coord_cartesian(ylim = c(0, y_upper)) +
    .distribution_plot_theme(style$font_size, show_legend = data$kind == "mixed")

  if (force_integer) {
    p <- p + ggplot2::scale_x_continuous(breaks = .integer_breaks)
  }

  if (data$kind == "mixed") {
    p <- p + .mixed_legend(style, show_density)
  }

  p
}



# Internal: build the data frame for bar layers ================================

#' @keywords internal
.distribution_plot_data <- function(data) {
  if (data$kind == "observed") {
    data.frame(
      value = data$observed,
      type  = factor("Observed", levels = c("Observed", "Imputed"))
    )
  } else if (data$kind == "implied") {
    data.frame(
      value = data$imputed,
      type  = factor("Imputed", levels = c("Observed", "Imputed"))
    )
  } else {
    data.frame(
      value = c(data$observed, data$imputed),
      type  = factor(
        c(rep("Observed", length(data$observed)),
          rep("Imputed",  length(data$imputed))),
        levels = c("Observed", "Imputed")
      )
    )
  }
}



# Internal: bar layers (filled, outlined, or both) =============================

#' @keywords internal
.bar_layers <- function(df, kind, binwidth, boundary, style) {
  layers <- list()

  if (kind %in% c("observed", "mixed")) {
    layers <- c(layers, list(
      ggplot2::geom_histogram(
        data     = subset(df, type == "Observed"),
        ggplot2::aes(y = ggplot2::after_stat(density)),
        binwidth = binwidth,
        boundary = boundary,
        fill     = style$observed_color,
        alpha    = style$fill_alpha
      )
    ))
  }

  if (kind %in% c("mixed", "implied")) {
    layers <- c(layers, list(
      ggplot2::geom_histogram(
        data      = subset(df, type == "Imputed"),
        ggplot2::aes(y = ggplot2::after_stat(density)),
        binwidth  = binwidth,
        boundary  = boundary,
        fill      = NA,
        color     = style$imputed_color,
        linewidth = style$line_width
      )
    ))
  }

  layers
}



# Internal: density layer ======================================================
#
# Smoothed density curve over all cases, weighted so each case contributes
# equally to the post-MI marginal distribution. Observed values get weight 1;
# imputed values get weight 1/M (since each missing case has M imputed values
# across iterations). Without weighting, the M-fold replication of imputed
# values would let them dominate the density estimate.
#
# For model-implied variables (.residual, .latent, .probability), all values
# are in `imputed` and each gets weight 1/M, which yields the average posterior
# density across cases.

#' @keywords internal
.density_layer <- function(observed, imputed, n_imps, bw, style) {
  density_data <- data.frame(
    value  = c(observed, imputed),
    weight = c(
      rep(1, length(observed)),
      rep(1 / n_imps, length(imputed))
    )
  )
  ggplot2::geom_density(
    data        = density_data,
    ggplot2::aes(x = value, weight = weight),
    color       = style$density_color,
    linewidth   = style$line_width,
    fill        = NA,
    bw          = bw,
    inherit.aes = FALSE
  )
}



# Internal: shared theme =======================================================

#' @keywords internal
.distribution_plot_theme <- function(font_size, show_legend) {
  base <- ggplot2::theme_minimal(base_size = font_size) +
    ggplot2::theme(
      panel.grid.major     = ggplot2::element_blank(),
      panel.grid.minor     = ggplot2::element_blank(),
      axis.text.y          = ggplot2::element_blank(),
      axis.ticks.y         = ggplot2::element_blank(),
      axis.text.x          = ggplot2::element_text(size = font_size),
      axis.title.x         = ggplot2::element_text(
                               size   = font_size,
                               margin = ggplot2::margin(t = 12)
                             ),
      axis.title.y         = ggplot2::element_text(size = font_size),
      plot.title           = ggplot2::element_text(
                               hjust = 0.5,
                               face  = "bold",
                               size  = font_size + 2
                             ),
      plot.subtitle        = ggplot2::element_text(
                               hjust = 0.5,
                               size  = font_size,
                               color = "grey40"
                             ),
      legend.text          = ggplot2::element_text(size = font_size + 2),
      legend.title         = ggplot2::element_text(size = font_size + 2),
      legend.key.size      = ggplot2::unit(1.5, "lines"),
      legend.key.height    = ggplot2::unit(1.5, "lines"),
      legend.key.width     = ggplot2::unit(2.0, "lines"),
      legend.spacing.x     = ggplot2::unit(0.8, "lines")
    )

  if (show_legend) {
    base + ggplot2::theme(
      legend.position      = "top",
      legend.justification = "left"
    )
  } else {
    base + ggplot2::theme(legend.position = "none")
  }
}



# Internal: manual two-color legend for the mixed case =========================

#' @keywords internal
.mixed_legend <- function(style, show_density = TRUE) {
  # Single dummy geom_point drives the legend. Entries render as filled
  # squares (shape 15), distinguished by color and alpha:
  #   Observed: filled with observed_color at fill_alpha (matches bar fill)
  #   Imputed:  filled with imputed_color at full opacity (matches outline)
  #   Combined: filled with density_color at full opacity (matches curve)
  # The Combined entry is included only when the density curve is drawn
  # (i.e., for non-discrete variables). For declared discrete variables
  # the density layer is suppressed and the legend has just two entries.
  #
  # The dummy points are invisible (alpha = 0, size = 0); the legend keys
  # are determined entirely by override.aes.
  if (show_density) {
    type_levels <- c("Observed", "Imputed", "Combined")
    legend_colors <- c(style$observed_color, style$imputed_color, style$density_color)
    legend_alphas <- c(style$fill_alpha, 1, 1)
  } else {
    type_levels <- c("Observed", "Imputed")
    legend_colors <- c(style$observed_color, style$imputed_color)
    legend_alphas <- c(style$fill_alpha, 1)
  }
  named_colors <- setNames(legend_colors, type_levels)

  legend_df <- data.frame(
    type = factor(type_levels, levels = type_levels),
    x    = 0,
    y    = 0
  )

  list(
    ggplot2::geom_point(
      data        = legend_df,
      ggplot2::aes(x = x, y = y, fill = type, color = type),
      size        = 0,
      alpha       = 0,
      inherit.aes = FALSE
    ),
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = named_colors,
      breaks = type_levels,
      labels = type_levels,
      guide  = ggplot2::guide_legend(
        override.aes = list(
          fill      = legend_colors,
          color     = legend_colors,
          alpha     = legend_alphas,
          linewidth = rep(0, length(type_levels)),
          size      = rep(6, length(type_levels)),
          shape     = rep(15, length(type_levels))
        )
      )
    ),
    ggplot2::scale_color_manual(
      name   = NULL,
      values = named_colors,
      breaks = type_levels,
      guide  = "none"
    )
  )
}



# Internal: display-label helper ===============================================
#
# Translates rblimp's terse variable names into reader-friendly labels. Used for
# axis labels and titles in BOTH distribution_plot and residuals_plot, so the
# same variable reads the same way everywhere. The bracketed multilevel forms in
# particular are spelled out rather than shown in raw `[cluster]` notation:
#   <dv>[cluster]        -> "<dv> Random Intercept"
#   <dv>$<iv>[cluster]   -> "<iv> Random Slope"
#   <x>.mean[cluster]    -> "<x> Latent Mean"       (a level-2 predictor)
#   <var>.residual       -> "<var> Residual"
#   <var>.latent         -> "<var> Latent Variable"
#   <var>.probability    -> "<var> Predicted Probability"
#   <var>.yjt            -> "<var> Yeo-Johnson Transformed"
# All other names pass through unchanged. Note the base variable name is left in
# its original case so the caller's `caps_axes`-style helpers can uppercase it;
# the descriptive words ("Random Intercept", etc.) are title case and left alone.

#' @keywords internal
.distribution_plot_xlab <- function(var) {
  # Bracketed multilevel quantities: spell out the random-effect meaning.
  if (.is_level2_var(var)) {
    if (grepl("\\$", var)) {                       # <dv>$<iv>[cluster]
      iv <- sub("^.+\\$(.+)\\[.+\\]$", "\\1", var)
      return(paste(iv, "Random Slope"))
    }
    if (.is_level2_mean(var)) {                    # <x>.mean[cluster]
      x <- sub("\\.mean\\[.+\\]$", "", var)
      return(paste(x, "Latent Mean"))
    }
    base <- sub("\\[.+\\]$", "", var)              # <dv>[cluster]
    return(paste(base, "Random Intercept"))
  }

  suffix_map <- c(
    residual    = "Residual",
    latent      = "Latent Variable",
    probability = "Predicted Probability",
    yjt         = "Yeo-Johnson Transformed"
  )
  parts <- strsplit(var, ".", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(var)
  suffix <- parts[length(parts)]
  if (!suffix %in% names(suffix_map)) return(var)
  base <- paste(parts[-length(parts)], collapse = ".")
  paste(base, suffix_map[[suffix]])
}



# Internal: binwidth helper ====================================================

#' @keywords internal
.distribution_plot_binwidth <- function(all_vals, bins) {
  if (.is_integer_var(all_vals)) return(1)
  rng <- range(all_vals, na.rm = TRUE)
  diff(rng) / bins
}



# Internal: boundary helper ====================================================

#' @keywords internal
.distribution_plot_boundary <- function(all_vals, binwidth) {
  data_min <- min(all_vals, na.rm = TRUE)
  if (.is_integer_var(all_vals)) return(floor(data_min) - 0.5)
  floor(data_min / binwidth) * binwidth
}



# Internal: integer-variable detection =========================================

#' @keywords internal
.is_integer_var <- function(x) {
  all(abs(x - round(x)) < 1e-6, na.rm = TRUE)
}



# Internal: effectively-discrete detection =====================================
#
# TRUE when a numeric column has so few distinct finite values that continuous-
# variable treatment (a KDE density curve, a normal Q-Q) is misleading -- e.g.
# a binary 0/1 indicator or a handful of Likert points. This is an EMPIRICAL
# backstop to the declared-discrete list (`.get_discrete_vars`): an indicator
# like `intschool` that is used in the model but never declared ORDINAL/NOMINAL
# would otherwise be treated as continuous and get a nonsensical two-band Q-Q.
# `max_unique` is the largest distinct-value count still treated as discrete
# (default matches the `few_values` convention used elsewhere: <= 5 distinct
# values is very likely categorical).

#' @keywords internal
.is_effectively_discrete <- function(x, max_unique = 5L) {
  x <- x[is.finite(x)]
  length(x) > 0L && length(unique(x)) <= max_unique
}



# Internal: Y-axis upper limit =================================================
#
# Computes the Y-axis upper limit as 1.10 times the maximum density across
# all visible layers (histogram bars and, if drawn, the density curve).
# Used by coord_cartesian() to clip the plot to the data range, preventing
# the manual-legend dummy points from inflating the Y axis.

#' @keywords internal
.distribution_plot_y_upper <- function(data, n_imps, binwidth, boundary, show_density, bw) {
  rng        <- range(c(data$observed, data$imputed), na.rm = TRUE)
  breaks_seq <- seq(boundary, rng[2] + binwidth, by = binwidth)

  obs_peak <- if (data$kind %in% c("observed", "mixed") && length(data$observed)) {
    max(hist(data$observed, breaks = breaks_seq, plot = FALSE)$counts /
          (length(data$observed) * binwidth))
  } else 0

  imp_peak <- if (data$kind %in% c("mixed", "implied") && length(data$imputed)) {
    max(hist(data$imputed, breaks = breaks_seq, plot = FALSE)$counts /
          (length(data$imputed) * binwidth))
  } else 0

  bar_peak <- max(obs_peak, imp_peak)

  # Weighted density peak (matches the weighting and bandwidth in .density_layer).
  # Explicit bw silences the "Selecting bandwidth *not* using 'weights'" warning.
  density_peak <- if (show_density) {
    weights <- c(
      rep(1, length(data$observed)),
      rep(1 / n_imps, length(data$imputed))
    )
    if (sum(weights) > 0) {
      weights <- weights / sum(weights)
      max(stats::density(c(data$observed, data$imputed), weights = weights, bw = bw)$y)
    } else 0
  } else 0

  max(bar_peak, density_peak) * 1.10
}



# Internal: integer-only X-axis breaks =========================================
#
# Used for variables declared as ORDINAL, NOMINAL, or COUNT in the script.
# For small integer ranges (<= 12 distinct values) shows every integer; for
# larger ranges, shows ~7 evenly-spaced integer ticks.

#' @keywords internal
.integer_breaks <- function(limits) {
  rng <- ceiling(limits[1]):floor(limits[2])
  if (length(rng) <= 12) return(rng)
  step <- ceiling(length(rng) / 7)
  seq(ceiling(limits[1]), floor(limits[2]), by = step)
}



# residuals_plot ===============================================================

#' Regression residual diagnostic plots
#'
#' Builds residual diagnostic plots for each outcome variable (DV) in an
#' rblimp model. The diagnostic suite branches on the DV's declared type
#' because the residual scale differs across model families:
#'
#' \describe{
#'   \item{Continuous outcomes}{Receive the full linear-style diagnostic
#'     suite: residual vs. predicted, residual vs. each predictor,
#'     standardized residual index, leverage, and Cook's distance.}
#'   \item{NOMINAL outcomes}{Receive a probability-scale binned-residuals
#'     plot (Gelman and Hill 2007) for each non-reference category, one
#'     per-predictor binned-residuals plot per predictor, and one
#'     Predicted Probability vs. Predictor overlay per predictor. The
#'     calibration plot bins cases by predicted probability; the
#'     per-predictor binned plots bin cases by predictor value (continuous
#'     predictors) or by observed level (discrete predictors); and the
#'     overlay plot displays the per-category predicted probabilities on
#'     a single panel, with each category distinguished by color, point
#'     shape, and line type. Linear-style diagnostics, the standardized
#'     residual index plot, leverage, and Cook's distance are not produced
#'     because NOMINAL outcomes are modeled with a Polya-Gamma latent
#'     response (Polson, Scott, and Windle 2013); the latent residuals are
#'     structurally bimodal because their sign is locked to the observed
#'     outcome, and standardizing by the auxiliary scale does not rectify
#'     them.}
#'   \item{ORDINAL outcomes}{Receive the same linear-style diagnostic
#'     suite as continuous outcomes (computed on the latent metric, where
#'     Albert-Chib augmentation yields approximately normal residuals),
#'     plus one Predicted Probability vs. Predictor overlay per predictor.
#'     The overlay displays the per-category predicted probabilities on a
#'     single panel, with each category distinguished by color, point
#'     shape, and line type. For ordinal outcomes the overlay corresponds
#'     to a category response curve display from item response theory:
#'     each curve shows P(Y = k) as a function of the predictor, with
#'     curve crossings marking the predictor values at which the dominant
#'     category switches.}
#'   \item{COUNT outcomes}{No diagnostics are produced. No latent-scale
#'     residual that supports a single coherent diagnostic display is
#'     currently exported.}
#' }
#'
#' Diagnostic families produced for continuous and ORDINAL outcomes:
#'
#' \describe{
#'   \item{Residual vs. predicted}{Scatter plot with a loess smoother.
#'     Targets nonlinearity and heteroscedasticity in the structural
#'     Y-X relationship.}
#'   \item{Standardized residual vs. each predictor}{One plot per predictor
#'     for each DV. Residuals are standardized by the model-implied
#'     per-observation \eqn{\sigma_i} when a variance model is fit
#'     (\eqn{\sigma_i = \sqrt{\exp(\mathrm{var}.dv.\mathrm{predicted}_i)}}),
#'     otherwise by the scalar marginal SD of the residuals. Continuous
#'     predictors get scatter + loess smoother; declared discrete
#'     predictors (ORDINAL, NOMINAL, COUNT) get stacked points at integer
#'     positions with a mean diamond at each level. Horizontal threshold
#'     lines at \eqn{\pm 2} and \eqn{\pm 3} match the index plot. Targets
#'     predictor-specific nonlinearity and residual variance heterogeneity;
#'     under a correctly specified variance model a fanning
#'     pattern collapses to a flat band.}
#'   \item{Standardized residual index}{Index plot of standardized residuals
#'     by case number, with horizontal threshold lines at \eqn{\pm 2} and
#'     \eqn{\pm 3}. Residuals are standardized casewise by the
#'     model-implied \eqn{\sigma_i} when a variance model is fit, otherwise
#'     by the scalar marginal SD. Labels case row numbers for cases with
#'     \eqn{|z| \ge 3}. Flags cases that are poorly predicted by the model.}
#'   \item{Leverage}{Index plot of leverage (diagonal of the hat matrix),
#'     with reference lines at \eqn{2p/n} and \eqn{3p/n}. Flags cases with
#'     extreme predictor values.}
#'   \item{Cook's distance}{Index plot of Cook's D, with reference lines
#'     at \eqn{4/n} and \eqn{1.0}. Flags cases that combine large
#'     residuals with high leverage.}
#' }
#'
#' All plots are computed from the *averaged* imputations: for each variable,
#' values are averaged across the M imputed datasets so that each case
#' contributes one residual, one predicted value, and one set of predictor
#' values. The averaging preserves the structural patterns these plots are
#' designed to detect (linearity, heteroscedasticity, outliers); a
#' Rubin-pooled version would be needed for inferential applications
#' requiring imputation-aware uncertainty bands.
#'
#' Leverage and Cook's distance use a closed-form computation on the
#' averaged design matrix and Blimp's averaged residuals. NOMINAL
#' predictors with \eqn{>2} levels are dummy-expanded with the lowest
#' level as the reference; ORDINAL, COUNT, and binary NOMINAL
#' predictors are used directly. The diagnostics are skipped for a DV
#' whose design matrix cannot be inverted.
#'
#' Returned plots are ggplot2 objects in a named list; the list has class
#' `"residuals_plot"` with an S3 print method that auto-displays each plot
#' when called interactively.
#'
#' @param model An rblimp model object. Must have non-empty `@imputations`
#'   and contain `<dv>.residual` and `<dv>.predicted` columns for each DV
#'   to be plotted.
#' @param vars Optional character vector of DV names. If `NULL` (default),
#'   plots every DV that has both `.residual` and `.predicted` columns.
#' @param font_size Numeric. Base font size in points. Default `14`.
#' @param point_color Color for scatter points. Default `"#00B0F6"` (blue).
#' @param curve_color Color for the smoother line. Default `"#F8766D"`
#'   (coral).
#' @param point_alpha Numeric in [0, 1]. Alpha for scatter points. Default
#'   `0.70`.
#' @param point_size Numeric. Point size in points. Default `1.5`.
#' @param line_width Numeric. Line weight for the smoother and reference
#'   lines. Default `1.0`.
#' @param index_thresholds Numeric vector. Horizontal reference lines
#'   drawn on the standardized residual index plot. Default
#'   `c(-3, 3)`. Cases are labeled only if `|z|` exceeds the
#'   largest absolute threshold (default: cases with `|z| >= 3`).
#' @param max_labels Integer. Maximum number of outlier labels drawn on
#'   the index, leverage, and Cook's distance plots. When more cases
#'   exceed the labeling threshold, only the `max_labels` most extreme
#'   cases (ranked by `|value|`) are labeled to keep large-sample plots
#'   legible. Default `10`.
#' @param label_family Character. Font family used for outlier labels
#'   and threshold annotations. Default `""` (graphics device default).
#'   Set to a family name (e.g., `"Minion Pro"`) to match a custom
#'   theme applied via `&` in patchwork.
#' @param few_values_warn Integer. For a predictor modeled as continuous, warn
#'   when it has this many or fewer distinct values (a likely sign it should
#'   have been declared ORDINAL or NOMINAL). The plot itself is left unchanged.
#'   Default `5`; set to `0` to disable the check.
#' @param verbose Logical. If `TRUE`, prints informational messages.
#'   Default `FALSE`.
#'
#' @return A named list of class `"residuals_plot"`. For continuous
#'   outcomes, names follow the pattern `"<dv>.predicted"`,
#'   `"<dv>.<predictor>"`, `"<dv>.index"`, `"<dv>.leverage"`, and
#'   `"<dv>.cooks"`. ORDINAL outcomes produce the same set plus one
#'   `"<dv>.<predictor>.probability"` entry per predictor (the Predicted
#'   Probability vs. Predictor overlay). NOMINAL outcomes produce
#'   `"<dv>.<k>.binned"` (calibration plot, one per non-reference
#'   category), `"<dv>.<k>.<predictor>"` (per-predictor binned plot,
#'   one per non-reference category per predictor), and
#'   `"<dv>.<predictor>.probability"` (one overlay per predictor). COUNT
#'   outcomes produce Pearson-residual diagnostics on the response scale:
#'   `"<dv>.index"` (Pearson residual index) and one `"<dv>.<predictor>"`
#'   (Pearson residual vs. predictor) per predictor; the linear-style
#'   residual-vs-predicted panel is omitted because the count latent residual
#'   is skewed.
#'
#'   When a DV has a level-2 (random-intercept) residual `<dv>[cluster]`, three
#'   further multilevel entries are added: `"<dv>[cluster].index"` (standardized
#'   level-2 residual index -- outlier clusters), one
#'   `"<dv>[cluster].<l2predictor>"` per level-2 predictor (random intercept vs.
#'   a cluster-level covariate), and `"<dv>.cluster_variance"` (within-cluster
#'   residual spread by cluster -- heterogeneity of the level-1 variance). The
#'   normal Q-Q of the level-2 residual is NOT here; it is produced by
#'   `distribution_plot` as `"<dv>[cluster].qq"`, beside its histogram. Note
#'   that level-2 (cluster-constant) predictors are plotted only against the
#'   level-2 residual, not the level-1 residual, so the `"<dv>.<predictor>"`
#'   level-1 panels use within-cluster predictors only (no duplicate panels).
#'   Leverage and Cook's distance (`"<dv>.leverage"`, `"<dv>.cooks"`) are
#'   emitted only for single-level DVs; they are OLS influence measures on the
#'   fixed-effects design and are omitted for a DV with a level-2 residual,
#'   because they do not account for the random-effects structure.
#'
#'   A level-2 latent random effect declared on the LATENT line and grouped by
#'   the clusterid (e.g. `b0c`/`b1c` in `person = b0c b1c`) is itself a level-2
#'   outcome: it receives `"<latent>.index"` and one `"<latent>.<l2predictor>"`
#'   per level-2 predictor (per-cluster diagnostics), and is NOT run through the
#'   level-1 suite. For a predictor centered at its latent group mean
#'   (`CENTER: groupmean = ...`), the level-1 `"<dv>.<predictor>"` panel plots the
#'   within-cluster deviation that enters the model, and its x-axis label says so.
#'
#'   These element names are the package's stable public interface: downstream
#'   code and manuscript figures index the list by name (e.g. `rp$dpdd.index`),
#'   so the naming scheme should be treated as fixed across versions.
#'
#'   Calling the list at the console
#'   auto-prints every plot in turn; assigning the result is silent.
#'
#' @examples
#' \dontrun{
#'   rp <- residuals_plot(my_model)
#'   rp$dpdd.predicted              # residual vs. predicted (continuous DV)
#'   rp$dpdd.inflam                 # residual vs. inflam     (continuous DV)
#'   rp$dpdd.index                  # standardized residual index
#'   rp$dpdd.leverage               # leverage index plot
#'   rp$dpdd.cooks                  # Cook's distance index plot
#'   rp$drinker.1.binned            # calibration plot (NOMINAL binary DV)
#'   rp$drinker.1.age               # binned residuals vs. age   (NOMINAL DV)
#'   rp$drinker.age.probability     # predicted prob overlay vs. age (NOMINAL DV)
#'   rp$drinkfreq.2.binned          # calibration plot (NOMINAL multi-cat DV)
#'   rp$drinkfreq.2.male            # binned residuals vs. male  (NOMINAL DV)
#'   rp$drinkfreq.age.probability   # predicted prob overlay vs. age (ORDINAL DV)
#' }
#'
#' @export
residuals_plot <- function(
  model,
  vars             = NULL,
  font_size        = 14,
  point_color      = "#00B0F6",
  curve_color      = "#F8766D",
  point_alpha      = 0.70,
  point_size       = 1.5,
  line_width       = 1.0,
  index_thresholds = c(-3, 3),
  max_labels       = 10,
  label_family     = "",
  few_values_warn  = 5,
  verbose          = FALSE
  # coral #F8766D olive #A3A500 mint #00BF7D blue #00B0F6 megenta #E76BF3
) {

  # Input validation
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!isS4(model) || !.hasSlot(model, "imputations")) {
    stop("`model` must be an rblimp model object.")
  }
  if (!is.list(model@imputations) || !length(model@imputations)) {
    stop("`model@imputations` must be a non-empty list.")
  }

  # Average imputations and detect what to plot. Residual diagnostics are
  # computed within each imputation and pooled (imp_list); avg_data is retained
  # for structural detection and for the nominal/count (binned/Pearson) panels.
  avg_data        <- .residuals_plot_average(model)
  imp_list        <- lapply(model@imputations, as.data.frame)

  # Level-2 (bracketed) random-effect residuals are diagnosed separately from
  # the level-1 DVs (see the level-2 block after the main loop). A bracketed
  # column earns level-2 *residual* diagnostics only when it is a DV's random
  # intercept: its base (the part before `[`) must itself be a plotted DV, i.e.
  # carry level-1 `.residual` + `.predicted`/`.probability` columns. That test
  # is what distinguishes `probsolve[school]` (the DV's level-2 residual) from
  # `stanmath.mean[school]` (a latent group mean -- a level-2 *predictor*, whose
  # base `stanmath.mean` is not a DV). Random-slope residuals (`<dv>$<x>[...]`)
  # are also level-2 residuals but are not diagnosed (the `$` guard skips
  # them). See `.is_dv_level2_residual`. Any user-supplied `vars` is split so
  # level-2 names bypass the level-1 `.residual`/`.predicted` detection, which
  # would otherwise error on them.
  l2_available <- Filter(
    function(nm) .is_dv_level2_residual(nm, names(imp_list[[1]])),
    names(imp_list[[1]])
  )
  if (is.null(vars)) {
    l2_resid <- l2_available
    vars_l1  <- NULL
    skip_l1  <- FALSE
  } else {
    l2_resid <- intersect(vars, l2_available)
    vars_l1  <- setdiff(vars, l2_available)
    skip_l1  <- !length(vars_l1)   # user requested only level-2 residuals
  }

  if (!skip_l1) {
    dv_bases        <- .residuals_plot_detect_dvs(avg_data, vars_l1, verbose)
    predictors_list <- .residuals_plot_predictors(model, dv_bases)
  } else {
    dv_bases        <- character(0)
    predictors_list <- list()
  }

  discrete_vars   <- .get_discrete_vars(model)
  nominal_vars    <- .get_discrete_vars(model, types = "nominal")
  ordinal_vars    <- .get_discrete_vars(model, types = "ordinal")
  count_vars      <- .get_discrete_vars(model, types = "count")

  # Cluster grouping and level-2 latent random effects, read from the model
  # SYNTAX rather than inferred from bracketed column names. `clusterid` is the
  # authoritative grouping (present even when the random intercept is a named
  # latent with no `<dv>[cluster]` column), and any latent declared on a LATENT
  # line grouped by that clusterid (e.g. `person = b0j b1j`) is a level-2 outcome
  # routed to the level-2 diagnostics below rather than the level-1 suite.
  cluster_ids   <- .model_clusterid(model)
  l2_latents    <- .model_level2_latents(model, cluster_ids)
  clusterid_col <- {
    present <- intersect(cluster_ids, names(imp_list[[1]]))
    if (length(present)) present[[1]] else NULL
  }
  # Predictors centered at their latent group means (CENTER: groupmean = ...).
  # In the level-1 residual-vs-predictor plots these are displayed as their
  # within-cluster deviation (raw minus `.mean[clusterid]`), which is the value
  # that actually enters the model. See the call site in the level-1 loop.
  groupmean_vars <- .model_groupmean_vars(model)

  # Common style bundle passed to every plot helper
  style <- list(
    font_size    = font_size,
    point_color  = point_color,
    curve_color  = curve_color,
    point_alpha  = point_alpha,
    point_size   = point_size,
    line_width   = line_width,
    max_labels      = max_labels,
    label_family    = label_family,
    few_values_warn = few_values_warn
  )

  plots <- list()
  overlay_done <- character(0)  # parents that have had their probability overlays generated
  for (dv in dv_bases) {

    # LEVEL-2 latent outcomes (random effects declared on the clusterid LATENT
    # line, e.g. b0j/b1j) are one-value-per-cluster quantities with their own
    # level-2 regression (`<name>.residual` / `.predicted`). Route them through
    # the level-2 diagnostics -- a per-cluster standardized residual index, plus
    # the level-2 residual against each level-2 predictor -- instead of the
    # per-row level-1 suite, which would treat each cluster's single value as if
    # it had n level-1 records (and emit OLS leverage/Cook's over duplicated
    # rows). The residual/cluster columns are passed explicitly because the
    # residual column (`b0j.residual`) is not itself bracketed.
    if (dv %in% names(l2_latents)) {
      cl_l2 <- l2_latents[[dv]]
      rc    <- paste0(dv, ".residual")
      if (rc %in% names(imp_list[[1]]) && cl_l2 %in% names(imp_list[[1]])) {
        idx <- .level2_residuals_index(imp_list, rc, index_thresholds, style,
                                       cluster_col = cl_l2, label = dv)
        if (!is.null(idx)) plots[[paste0(dv, ".index")]] <- idx

        for (pcol in .level2_predictor_cols(imp_list[[1]], model, dv, cl_l2)) {
          pred_base <- sub("\\[.+\\]$", "", pcol)
          pv <- .level2_residual_vs_predictor(
            imp_list, rc, pcol, pred_base %in% discrete_vars, style,
            cluster_col = cl_l2, label = dv
          )
          if (!is.null(pv)) plots[[paste0(dv, ".", pcol)]] <- pv
        }
      } else if (verbose) {
        message("Skipping level-2 diagnostics for latent `", dv,
                "`: residual or cluster column missing.")
      }
      next
    }

    outcome <- .classify_outcome_type(dv, ordinal_vars, nominal_vars, count_vars)

    # NOMINAL and ORDINAL outcomes: build one Predicted Probability vs.
    # Predictor overlay per predictor (per parent, deduplicated so multi-
    # category NOMINAL DVs do not generate the same overlay K - 1 times).
    if (outcome$type %in% c("nominal", "ordinal") &&
        !outcome$parent %in% overlay_done) {
      for (pred in predictors_list[[dv]]) {
        if (is.na(.resolve_predictor_col(imp_list[[1]], pred))) {
          if (verbose) message("Skipping predictor not found in data: ", pred)
          next
        }
        pp <- .predicted_probability_vs_predictor(
          imp_list, outcome$parent, pred, pred %in% discrete_vars, style
        )
        if (!is.null(pp)) {
          plots[[paste0(outcome$parent, ".", pred, ".probability")]] <- pp
        }
      }
      overlay_done <- c(overlay_done, outcome$parent)
    }

    # COUNT outcomes: Pearson residual diagnostics on the response scale
    # (Gelman and Hill 2006, Section 6.2). Linear-style residual-vs-predicted
    # plots are skipped because the PG-NB latent residual is structurally
    # right-skewed; the response-scale Pearson residual sidesteps that.
    if (outcome$type == "count") {
      alpha <- model@estimates[
        paste0(outcome$parent, " Variance Dispersion Parameter"),
        "Estimate"
      ]

      # Pearson residual index plot
      plots[[paste0(outcome$parent, ".index")]] <-
        .pearson_residuals_index(imp_list, outcome$parent, alpha,
                                 index_thresholds, style)

      # Pearson residual vs. each predictor
      for (pred in predictors_list[[dv]]) {
        if (is.na(.resolve_predictor_col(imp_list[[1]], pred))) {
          if (verbose) message("Skipping predictor not found in data: ", pred)
          next
        }
        plots[[paste0(outcome$parent, ".", pred)]] <-
          .pearson_residuals_vs_predictor(
            imp_list, outcome$parent, pred, alpha,
            pred %in% discrete_vars, style
          )
      }
      next
    }

    # NOMINAL outcomes: probability-scale binned residuals (Gelman & Hill 2007).
    # The Polya-Gamma latent residual is structurally bimodal (its sign is
    # locked to Y), so linear-style diagnostics are skipped. Leverage and
    # Cook's distance are also skipped because they would inherit the same
    # latent-scale problem. The standardized residual index plot has no
    # meaningful binary analogue and is intentionally omitted.
    if (outcome$type == "nominal") {
      # Calibration plot: binned residuals vs. predicted probability
      bp <- .binned_residuals(imp_list, outcome$parent, outcome$category, style)
      if (!is.null(bp)) {
        plots[[paste0(dv, ".binned")]] <- bp
      } else if (verbose) {
        message("Skipping binned residuals for `", dv,
                "`: too few cases or missing probability column.")
      }

      # Binned residuals vs. each predictor (one plot per predictor)
      for (pred in predictors_list[[dv]]) {
        if (is.na(.resolve_predictor_col(imp_list[[1]], pred))) {
          if (verbose) message("Skipping predictor not found in data: ", pred)
          next
        }
        bvp <- .binned_residuals_vs_predictor(
          imp_list, outcome$parent, outcome$category, pred,
          pred %in% discrete_vars, style
        )
        if (!is.null(bvp)) {
          plots[[paste0(dv, ".", pred)]] <- bvp
        }
      }
      next
    }

    # Continuous and ORDINAL outcomes: linear-style diagnostic suite.
    # Residual vs. predicted (pooled across imputations)
    plots[[paste0(dv, ".predicted")]] <-
      .residuals_vs_predicted(imp_list, dv, style)

    # Cluster-ID columns for this DV's level-2 residual(s), if any. A LEVEL-1
    # residual-vs-predictor plot should only use level-1 (within-cluster)
    # predictors, so any predictor that is constant within cluster (a level-2
    # covariate such as a raw cluster-level variable) is skipped here -- it is
    # plotted in the multilevel block against the level-2 residual instead,
    # avoiding a duplicate panel. For a single-level DV there are no clusters
    # and nothing is skipped.
    dv_l2   <- Filter(function(nm) .is_dv_level2_residual(nm, names(imp_list[[1]])) &&
                        sub("\\[.+\\]$", "", nm) == dv, names(imp_list[[1]]))
    dv_clusters <- unique(.cluster_id_of(dv_l2))

    # Residual vs. each predictor (pooled across imputations)
    for (pred in predictors_list[[dv]]) {
      pcol <- .resolve_predictor_col(avg_data, pred)
      if (is.na(pcol)) {
        if (verbose) message("Skipping predictor not found in data: ", pred)
        next
      }
      # Skip higher-level (cluster-constant) predictors at level 1.
      is_l2 <- length(dv_clusters) > 0 && any(vapply(dv_clusters, function(cl)
        cl %in% names(imp_list[[1]]) &&
          .is_cluster_constant(imp_list[[1]][[pcol]], imp_list[[1]][[cl]]),
        logical(1)))
      if (is_l2) {
        if (verbose) message("Skipping level-2 predictor at level 1 (plotted vs. the level-2 residual): ", pred)
        next
      }
      # Group-mean-centered level-1 predictor: display the within-cluster
      # deviation (raw minus `<pred>.mean[clusterid]`) that enters the model.
      # Only applies to predictors on the `groupmean =` line whose mean column is
      # present; everything else keeps the raw predictor.
      center_col <- NULL
      if (pred %in% groupmean_vars && !is.null(clusterid_col)) {
        mcol <- paste0(pred, ".mean[", clusterid_col, "]")
        if (mcol %in% names(imp_list[[1]])) center_col <- mcol
      }
      plots[[paste0(dv, ".", pred)]] <-
        .residuals_vs_predictor(imp_list, dv, pred,
                                pred %in% discrete_vars,
                                index_thresholds, style, center_col = center_col)
    }

    # Standardized residual index (all imputations shown per case)
    plots[[paste0(dv, ".index")]] <-
      .residuals_index(imp_list, dv, index_thresholds, style)

    # Within-cluster residual-variance heterogeneity (continuous outcomes only).
    # This is the level-1 DV's cluster diagnostic, keyed on the model's
    # clusterid. It is produced HERE for a clustered DV that has NO bracketed
    # `<dv>[cluster]` random-intercept column -- e.g. a multilevel model whose
    # random intercept is a named latent (`posaff` with intercept `@b0j`), so the
    # level-2 block below never fires for it. A DV that DOES have a bracketed
    # residual gets this same plot from the level-2 block instead, so the
    # `!length(dv_clusters)` guard prevents a duplicate panel. Bernoulli/ordinal/
    # count outcomes are skipped (no free level-1 residual variance), matching the
    # level-2 block's gate.
    if (!is.null(clusterid_col) && !length(dv_clusters) &&
        outcome$type == "continuous") {
      het <- .residuals_by_cluster(imp_list, dv, clusterid_col, style)
      if (!is.null(het)) plots[[paste0(dv, ".cluster_variance")]] <- het
    }

    # Leverage and Cook's distance (per imputation, averaged per case). These
    # are OLS influence measures computed on the FIXED-EFFECTS design matrix
    # only; they ignore the random-effects structure (Z), the within-cluster
    # correlation, and cluster-level influence, and they pair an OLS hat matrix
    # with the model's conditional (level-1) residual -- an inconsistent
    # combination. They are therefore emitted ONLY for genuinely single-level
    # DVs. A DV is multilevel if it has a bracketed level-2 residual
    # (`dv_clusters`) OR the model declares a `clusterid` (which also catches a
    # named-latent random intercept that produces no `<dv>[cluster]` column --
    # the case the earlier `!length(dv_clusters)` test alone missed). Valid
    # multilevel influence requires cluster-deletion refitting, which cannot be
    # reconstructed from saved imputations.
    is_multilevel <- length(dv_clusters) > 0 || !is.null(clusterid_col)
    if (!is_multilevel) {
      diag <- .residuals_lm_diagnostics(
        imp_list, dv, .model_design_terms(model, dv), nominal_vars, verbose,
        .center_constants(model)
      )
      if (!is.null(diag)) {
        plots[[paste0(dv, ".leverage")]] <- .residuals_leverage(diag, dv, style)
        plots[[paste0(dv, ".cooks")]]    <- .residuals_cooks(diag, dv, style)
      }
    } else if (verbose) {
      message("Skipping leverage/Cook's D for multilevel DV `", dv,
              "`: OLS influence measures do not account for the random-effects structure.")
    }
  }

  # -------------------------------------------------------------------------
  # Multilevel diagnostics. For each DV that has a level-2 residual
  # `<dv>[cluster]`, three things are produced. NB: the normal Q-Q of the
  # cluster residuals is intentionally NOT here -- it is a univariate
  # distributional display and lives in distribution_plot() (as
  # `<dv>[cluster].qq`), alongside the histogram of the same values.
  #
  #   (a) Standardized level-2 residual index plot -- flags outlier clusters
  #       (unusually high/low random intercepts). The level-1 residual-vs-
  #       predicted / leverage / Cook's machinery does NOT transfer: a level-2
  #       residual has no per-cluster fitted value and no per-cluster design
  #       matrix. See `.level2_residuals_index`.
  #
  #   (b) Level-2 residual vs. each level-2 predictor -- checks whether the
  #       random intercept co-varies with a cluster-level covariate (an omitted
  #       contextual effect / violation of independence of random effects).
  #       Only *level-2* predictors are eligible: a predictor is level-2 iff it
  #       is (i) on the DV's MODEL line and (ii) constant within every cluster.
  #       Condition (i) excludes the non-predictor `.mean[cluster]` columns
  #       Blimp auto-generates; condition (ii) keeps both latent group means
  #       (`x.mean[cluster]`) and raw cluster-level covariates (e.g.
  #       `intschool`). See `.level2_predictor_cols` and
  #       `.level2_residual_vs_predictor`.
  #
  #   (c) Within-cluster residual-variance heterogeneity -- one plot of the
  #       level-1 residuals, standardized by a single pooled SD, boxed by
  #       cluster. Detects whether within-cluster variance differs across
  #       clusters (motivating a cluster-specific-variance model). See
  #       `.residuals_by_cluster` and the note in that function for why the
  #       standardizer is the pooled SD and NOT each cluster's own SD.
  # -------------------------------------------------------------------------
  for (v in l2_resid) {
    dv <- sub("\\[.+\\]$", "", v)           # DV base, e.g. "probsolve"
    cl <- .cluster_id_of(v)                  # cluster-ID column, e.g. "school"

    # (a) standardized level-2 residual index
    idx <- .level2_residuals_index(imp_list, v, index_thresholds, style)
    if (!is.null(idx)) {
      plots[[paste0(v, ".index")]] <- idx
    } else if (verbose) {
      message("Skipping level-2 index for `", v,
              "`: cluster column missing or fewer than 2 clusters.")
    }

    # (b) level-2 residual vs. each level-2 predictor
    l2_preds <- .level2_predictor_cols(imp_list[[1]], model, dv, cl)
    for (pcol in l2_preds) {
      # Declared-discrete status of the predictor (base name, bracket stripped),
      # passed so the loess/diamond decision matches level-1 exactly: declared
      # discrete OR <= 10 distinct values -> category-mean diamonds.
      pred_base   <- sub("\\[.+\\]$", "", pcol)
      pv <- .level2_residual_vs_predictor(
        imp_list, v, pcol, pred_base %in% discrete_vars, style
      )
      if (!is.null(pv)) plots[[paste0(v, ".", pcol)]] <- pv
    }

    # (c) within-cluster variance heterogeneity -- CONTINUOUS outcomes only.
    # This plot standardizes the level-1 residual and reads its spread as the
    # level-1 residual variance. That only makes sense when the outcome has a
    # free, homogeneous residual variance to be heterogeneous across clusters.
    # For a NOMINAL/binary (Bernoulli), ORDINAL (fixed latent scale), or COUNT
    # outcome the level-1 variance is pinned to the mean, so there is no sigma^2
    # to standardize by or to compare across clusters -- the plot is skipped,
    # the same way those outcomes already skip residual-vs-predicted / leverage /
    # Cook's. (The level-2 residual index and vs-predictor plots above DO apply:
    # the random intercept is a continuous, normal level-2 quantity regardless of
    # the outcome's scale.)
    het_outcome <- .classify_outcome_type(dv, ordinal_vars, nominal_vars, count_vars)
    if (het_outcome$type == "continuous") {
      het <- .residuals_by_cluster(imp_list, dv, cl, style)
      if (!is.null(het)) {
        plots[[paste0(dv, ".cluster_variance")]] <- het
      } else if (verbose) {
        message("Skipping cluster-variance plot for `", dv,
                "`: residual or cluster column missing.")
      }
    } else if (verbose) {
      message("Skipping cluster-variance plot for `", dv,
              "`: only defined for continuous outcomes (a ", het_outcome$type,
              " outcome has no free level-1 residual variance).")
    }
  }

  class(plots) <- c("residuals_plot", "list")
  plots
}



# print method =================================================================

#' @export
print.residuals_plot <- function(x, ...) {
  for (p in x) print(p)
  invisible(x)
}



# Internal: averaged imputations ===============================================
#
# Returns Blimp's canonical post-MI summary as a data frame. Continuous
# variables hold the arithmetic mean across imputations; multicategorical
# variables hold the modal imputation, so categorical columns retain
# integer category codes rather than collapsing to a between-category
# fraction. Errors if the slot is missing or empty.

#' @keywords internal
.residuals_plot_average <- function(model) {
  if (!.hasSlot(model, "average_imp") ||
      is.null(model@average_imp) ||
      !length(model@average_imp)) {
    stop("`model@average_imp` is missing or empty. Re-run the model with a ",
         "current version of rblimp to populate the averaged-imputation slot.")
  }
  as.data.frame(model@average_imp)
}



# Internal: detect DVs with .residual paired with .predicted or .probability ==
#
# Continuous and ORDINAL outcomes pair `<dv>.residual` with `<dv>.predicted`.
# NOMINAL outcomes pair `<dv>.<k>.residual` with `<dv>.<k>.probability`
# (one entry per non-reference category, no .predicted column). The detection
# accepts either pairing; the main loop then dispatches by outcome type.

#' @keywords internal
.residuals_plot_detect_dvs <- function(data, vars, verbose) {
  cols       <- names(data)
  resid_cols <- grep("\\.residual$", cols, value = TRUE)
  bases      <- sub("\\.residual$", "", resid_cols)

  # Keep DVs that have either .predicted (continuous/ordinal path) or
  # .probability (nominal path). Bases with neither are dropped.
  has_predicted   <- paste0(bases, ".predicted")   %in% cols
  has_probability <- paste0(bases, ".probability") %in% cols
  bases <- bases[has_predicted | has_probability]

  if (!is.null(vars)) {
    missing_vars <- setdiff(vars, bases)
    if (length(missing_vars)) {
      stop("Variable(s) not found with .residual and .predicted/.probability columns: ",
           paste(missing_vars, collapse = ", "))
    }
    bases <- intersect(bases, vars)
  }

  if (!length(bases)) {
    stop("No DVs found with .residual paired with .predicted or .probability.")
  }
  if (verbose) {
    message("DVs to plot: ", paste(bases, collapse = ", "))
  }
  bases
}



# Internal: classify a DV base by declared outcome type ========================
#
# Returns a list with three components describing how the DV should be
# diagnosed:
#
#   type     -- one of "continuous", "ordinal", "nominal", "count".
#   parent   -- the parent variable name (e.g., "drinker" for the nominal
#               base "drinker.1"; the dv_base itself otherwise).
#   category -- integer category index for NOMINAL outcomes
#               (e.g., 1 for "drinker.1"); NA otherwise.
#
# The function checks for a direct match against the declared discrete-type
# vectors first; if no match, it parses the dv_base for the trailing
# "<parent>.<k>" pattern produced by NOMINAL declarations and re-checks the
# parent name. DVs that match nothing are treated as continuous.

#' @keywords internal
.classify_outcome_type <- function(dv_base, ordinal_vars, nominal_vars, count_vars) {
  if (dv_base %in% nominal_vars)
    return(list(type = "nominal", parent = dv_base, category = NA_integer_))
  if (dv_base %in% ordinal_vars)
    return(list(type = "ordinal", parent = dv_base, category = NA_integer_))
  if (dv_base %in% count_vars)
    return(list(type = "count",   parent = dv_base, category = NA_integer_))

  # Try the "<parent>.<k>" pattern: split on the last dot and check if the
  # tail is an integer and the head is a declared variable.
  parts <- strsplit(dv_base, ".", fixed = TRUE)[[1]]
  if (length(parts) >= 2) {
    last <- parts[length(parts)]
    k    <- suppressWarnings(as.integer(last))
    if (!is.na(k)) {
      parent <- paste(parts[-length(parts)], collapse = ".")
      if (parent %in% nominal_vars)
        return(list(type = "nominal", parent = parent, category = k))
      if (parent %in% ordinal_vars)
        return(list(type = "ordinal", parent = parent, category = k))
      if (parent %in% count_vars)
        return(list(type = "count",   parent = parent, category = k))
    }
  }

  list(type = "continuous", parent = dv_base, category = NA_integer_)
}



# Internal: parse MODEL block to extract predictors per DV =====================
#
# Returns a named list mapping each DV in `dv_bases` to a vector of its
# main-effect predictors. Random-slope predictors (after `|`) and interaction
# terms (`x*y`) are dropped; parameter labels (`x@b1`) are stripped.

#' @keywords internal
.residuals_plot_predictors <- function(model, dv_bases) {
  mb <- .get_model_block(model)
  result <- setNames(vector("list", length(dv_bases)), dv_bases)

  if (!length(mb)) return(result)

  # Strip inline comments (Blimp uses `#` to end of line) before tokenizing,
  # in case the stored syntax retains them. Split to physical lines first so
  # the strip respects line boundaries whether `mb` is one multi-line string
  # or a vector of lines.
  mb <- unlist(strsplit(paste(mb, collapse = "\n"), "\n", fixed = TRUE),
               use.names = FALSE)
  mb <- sub("#.*$", "", mb)

  # Derive each base's parent name. For continuous and ORDINAL bases the
  # parent is the base itself; for NOMINAL-indexed bases like "drinker.1"
  # the parent is the part before the trailing ".<k>". A MODEL line with
  # DV "drinker" should attach its predictors to every indexed base whose
  # parent is "drinker".
  base_parents <- vapply(dv_bases, function(b) {
    parts <- strsplit(b, ".", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      last <- parts[length(parts)]
      k    <- suppressWarnings(as.integer(last))
      if (!is.na(k)) return(paste(parts[-length(parts)], collapse = "."))
    }
    b
  }, character(1))

  chunks <- unlist(strsplit(paste(mb, collapse = " "), ";", fixed = TRUE),
                   use.names = FALSE)
  chunks <- gsub("\\s+", " ", trimws(chunks))
  # Normalize Unicode dashes (figure/en/em dash, horizontal bar, minus sign)
  # to ASCII "-" so the loading arrow is recognized even when an editor has
  # auto-converted the ASCII "->" into a Unicode-dash form (e.g. an en-dash
  # followed by ">"). Blimp accepts these variants; this lets the parser match
  # them too.
  chunks <- gsub("[\u2012\u2013\u2014\u2015\u2212]", "-", chunks)
  chunks <- chunks[nzchar(chunks)]

  for (ch in chunks) {
    # Factor / loading syntax: `<predictor(s)> -> <outcome(s)>`. Blimp regresses
    # each RHS outcome on the LHS predictor(s) -- the measurement-model analogue
    # of `outcome ~ predictor` with the sides reversed. A latent factor `f` is
    # stored in the averaged data as `f.latent`; `.resolve_predictor_col` maps
    # the bare name to that column at lookup time. Because Blimp imputes the
    # factor, leverage and Cook's D are well defined against the `.latent`
    # score just as they are for any observed predictor.
    if (grepl("->", ch, fixed = TRUE)) {
      sides <- strsplit(ch, "->", fixed = TRUE)[[1]]
      if (length(sides) != 2) next

      lhs_toks <- strsplit(trimws(sides[1]), "\\s+")[[1]]
      lhs_toks <- sub("@.+$", "", lhs_toks)                # strip @label
      lhs_toks <- lhs_toks[!grepl(":$", lhs_toks)]         # drop block labels e.g. "measurement:"
      lhs_toks <- lhs_toks[nzchar(lhs_toks) & !grepl("\\*", lhs_toks)]
      lhs_toks <- unique(lhs_toks)

      out_toks <- strsplit(trimws(sides[2]), "\\s+")[[1]]
      out_toks <- sub("@.+$", "", out_toks)                # strip @label
      out_toks <- out_toks[nzchar(out_toks)]

      for (out in out_toks) {
        out <- sub("^yjt\\((.*)\\)$", "\\1", out)
        matched_bases <- dv_bases[base_parents == out]
        for (base in matched_bases) {
          result[[base]] <- unique(c(result[[base]], lhs_toks))
        }
      }
      next
    }

    if (!grepl("~", ch, fixed = TRUE)) next
    parts <- strsplit(ch, "~", fixed = TRUE)[[1]]
    if (length(parts) != 2) next

    # DV: extract the variable name from the LHS. Drop a block-label prefix
    # (e.g. "focal.model:"), unwrap a yjt(...) transform wrapper, then take the
    # first identifier and ignore any in-formula centering/arithmetic. Blimp
    # names the model-implied columns after that variable and drops the
    # arithmetic, so "yjt(dpdd - 6) ~ ..." produces dpdd.residual /
    # dpdd.predicted, and the DV here must reduce to "dpdd" to match them.
    dv_raw <- trimws(parts[1])
    dv_raw <- sub("^\\s*[A-Za-z0-9._]+:\\s*", "", dv_raw)    # drop "block:" label
    dv_raw <- sub("^yjt\\((.*)\\)$", "\\1", trimws(dv_raw))  # unwrap yjt(...)
    dv     <- regmatches(dv_raw, regexpr("[A-Za-z][A-Za-z0-9._]*", dv_raw))
    if (!length(dv) || !nzchar(dv)) next

    # Match the model-line DV against every base whose parent equals dv.
    # This covers continuous/ORDINAL (dv matches its own base) and NOMINAL
    # (dv matches every indexed base "dv.k").
    matched_bases <- dv_bases[base_parents == dv]
    if (!length(matched_bases)) next

    # Predictors: main RHS only (drop random-slope segment after `|`)
    rhs        <- strsplit(trimws(parts[2]), "\\|", fixed = TRUE)[[1]][1]
    pred_toks  <- strsplit(trimws(rhs), "\\s+")[[1]]
    pred_toks  <- pred_toks[nzchar(pred_toks)]
    pred_toks  <- sub("@.+$", "", pred_toks)            # strip @label
    pred_toks  <- pred_toks[!grepl("\\*", pred_toks)]   # drop interactions
    pred_toks  <- unique(pred_toks)

    for (base in matched_bases) {
      result[[base]] <- pred_toks
    }
  }
  result
}



# Internal: cluster ID(s) from the model syntax ===============================
#
# Returns the clusterid variable name(s) declared on rblimp's `clusterid` line,
# or NULL for a single-level model. This is the AUTHORITATIVE source of the
# cluster grouping -- unlike the bracketed-name heuristic (`.is_level2_var`), it
# is present even when a model has no bracketed columns at all (e.g. a random
# intercept declared as a named latent variable rather than the auto-generated
# `<dv>[cluster]`). A 3-level model may list more than one id; all are returned.

#' @keywords internal
.model_clusterid <- function(model) {
  sx  <- model@syntax
  cid <- if ((inherits(sx, "blimp_syntax") || is.list(sx)) &&
             "clusterid" %in% names(sx)) sx$clusterid else NULL
  if (is.null(cid)) return(NULL)
  toks <- unlist(strsplit(paste(as.character(cid), collapse = " "), "\\s+"),
                 use.names = FALSE)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) NULL else toks
}



# Internal: level-2 latent random effects from the LATENT line ================
#
# In a multilevel model the random effects are declared on a LATENT line whose
# left-hand side is the clusterid, e.g. `person = b0j b1j`. That LHS is what
# marks the right-hand-side latents as LEVEL-2 quantities: the same `latent =`
# syntax also declares ordinary latent variables in single-level models, so a
# latent is level-2 ONLY when its declaration is grouped by a clusterid (which
# is why the LHS is tested against `clusterids`, and why `center` lines like
# `groupmean = pain` -- whose LHS is not a clusterid -- are correctly ignored).
# Blimp stores each such latent as `<name>.latent` with companion
# `<name>.residual` / `<name>.predicted` columns from its own level-2 regression,
# so residuals_plot routes these through the level-2 diagnostics rather than the
# per-row level-1 suite (which would treat one-value-per-cluster quantities as if
# they had n level-1 rows and emit invalid OLS influence measures). Returns a
# named character vector mapping latent name -> clusterid (empty if none).

#' @keywords internal
.model_level2_latents <- function(model, clusterids) {
  if (!length(clusterids)) return(character(0))
  sx  <- model@syntax
  lat <- if ((inherits(sx, "blimp_syntax") || is.list(sx)) &&
             "latent" %in% names(sx)) as.character(sx$latent) else NULL
  if (!length(lat)) return(character(0))

  # Split into individual `<group> = <vars>` declarations (entries may be one
  # string, a character vector, or semicolon/newline separated).
  pieces <- unlist(strsplit(paste(lat, collapse = "\n"), "[;\n]"), use.names = FALSE)
  out <- character(0)
  for (p in pieces) {
    if (!grepl("=", p, fixed = TRUE)) next
    lhs <- trimws(sub("=.*$", "", p))
    if (!lhs %in% clusterids) next          # only clusterid-grouped latents are level-2
    rhs  <- trimws(sub("^[^=]*=", "", p))
    vars <- unlist(strsplit(rhs, "\\s+"), use.names = FALSE)
    vars <- vars[nzchar(vars)]
    for (v in vars) out[v] <- lhs
  }
  out
}



# Internal: group-mean-centered predictors from the CENTER line ===============
#
# Returns the predictor names listed on a `CENTER: groupmean = ...` declaration
# (empty if none). Such a predictor enters the level-1 model as its within-
# cluster deviation, `<pred> - <pred>.mean[clusterid]`, so the level-1
# residual-vs-predictor plot displays that centered value rather than the raw
# score (see the `center_col` path in `.residuals_vs_predictor`). Only the
# `groupmean =` line is read; `grandmean =` and other CENTER lines are ignored,
# and `#` comments are stripped first (a CENTER block may comment a line out).

#' @keywords internal
.model_groupmean_vars <- function(model) {
  sx  <- model@syntax
  cen <- if ((inherits(sx, "blimp_syntax") || is.list(sx)) &&
             "center" %in% names(sx)) as.character(sx$center) else NULL
  if (!length(cen)) return(character(0))

  lines  <- unlist(strsplit(paste(cen, collapse = "\n"), "\n", fixed = TRUE),
                   use.names = FALSE)
  lines  <- sub("#.*$", "", lines)                  # strip comments
  pieces <- unlist(strsplit(paste(lines, collapse = " "), ";", fixed = TRUE),
                   use.names = FALSE)
  out <- character(0)
  for (p in pieces) {
    if (!grepl("=", p, fixed = TRUE)) next
    if (tolower(trimws(sub("=.*$", "", p))) != "groupmean") next
    rhs  <- trimws(sub("^[^=]*=", "", p))
    vars <- unlist(strsplit(rhs, "\\s+"), use.names = FALSE)
    out  <- c(out, vars[nzchar(vars)])
  }
  unique(out)
}



# Internal: multiple-imputation pooling helpers ================================
#
# Residual diagnostics are computed WITHIN each imputed data set and pooled,
# rather than computed once on @average_imp. Averaging the residual and the
# imputed latent first induces a spurious residual-predictor association (a
# linear tilt and exaggerated curvature); estimating within each imputation and
# pooling removes it. Points are stacked across imputations (honest imputation
# uncertainty); smoother curves are fit per imputation and averaged (Rubin
# point-estimate order); leverage/Cook's D are computed per imputation and
# averaged per case, with Cook's D scaled by the single pooled error variance
# (`.pooled_sigma`) rather than each imputation's own, so its variance benchmark
# matches the standardized residual index.

#' @keywords internal
.pooled_sigma <- function(imp_list, dv) {
  resid_col <- paste0(dv, ".residual")
  var_col   <- paste0("var.", dv, ".predicted")
  n         <- nrow(imp_list[[1]])
  if (var_col %in% names(imp_list[[1]])) {
    rowMeans(vapply(imp_list, function(d) sqrt(exp(d[[var_col]])), numeric(n)))
  } else {
    s <- mean(vapply(imp_list,
                     function(d) stats::sd(d[[resid_col]], na.rm = TRUE),
                     numeric(1)), na.rm = TRUE)
    rep(s, n)
  }
}

# Effective point alpha for a stacked cloud of M imputations. Reduces alpha so
# the dense centre does not fully saturate, but only gently (cube root of M) so
# sparse extreme points stay visible. Tunable via style$point_alpha.
#' @keywords internal
.pooled_alpha <- function(point_alpha, M) point_alpha / max(M, 1)^(1 / 3)

# Stacked per-imputation (x, residual/sigma) points. `xfun` extracts the x
# column from one imputation; `sigma` is the pooled per-observation SD (pass a
# vector of 1s for raw residuals).
#' @keywords internal
.pooled_xy <- function(imp_list, dv, xfun, sigma) {
  rc  <- paste0(dv, ".residual")
  pts <- do.call(rbind, lapply(imp_list, function(d)
    data.frame(x = xfun(d), y = d[[rc]] / sigma)))
  pts[is.finite(pts$x) & is.finite(pts$y), ]
}

# Fit-then-pool loess: fit within each imputation, predict on a common trimmed
# grid, average the curves. No confidence band (stacked draws are not
# independent, and the function shows no bands anyway).
#' @keywords internal
.pooled_loess_curve <- function(imp_list, dv, xfun, sigma, span = 0.75,
                                n_grid = 200) {
  rc   <- paste0(dv, ".residual")
  allx <- unlist(lapply(imp_list, xfun), use.names = FALSE)
  rng  <- stats::quantile(allx, c(0.01, 0.99), na.rm = TRUE)
  grid <- seq(rng[1], rng[2], length.out = n_grid)
  preds <- vapply(imp_list, function(d) {
    df <- data.frame(x = xfun(d), y = d[[rc]] / sigma)
    df <- df[is.finite(df$x) & is.finite(df$y), ]
    if (nrow(df) < 10 || length(unique(df$x)) < 4) {
      return(rep(NA_real_, length(grid)))
    }
    fit <- tryCatch(stats::loess(y ~ x, df, span = span), error = function(e) NULL)
    if (is.null(fit)) return(rep(NA_real_, length(grid)))
    suppressWarnings(stats::predict(fit, newdata = data.frame(x = grid)))
  }, numeric(length(grid)))
  data.frame(x = grid, y = rowMeans(preds, na.rm = TRUE))
}

# Fit-then-pool loess from a list of per-imputation (x, y) data frames whose x
# and y are already computed (e.g. probability-scale binned residuals or Pearson
# residuals, where there is no single ".residual" column to divide). Fits within
# each imputation, predicts on a common trimmed grid, averages the curves.
#' @keywords internal
.pooled_loess_xy <- function(xy_list, span = 0.75, n_grid = 200) {
  allx <- unlist(lapply(xy_list, function(d) d$x), use.names = FALSE)
  allx <- allx[is.finite(allx)]
  if (length(allx) < 2) return(data.frame(x = numeric(0), y = numeric(0)))
  rng  <- stats::quantile(allx, c(0.01, 0.99), na.rm = TRUE)
  grid <- seq(rng[1], rng[2], length.out = n_grid)
  preds <- vapply(xy_list, function(d) {
    d <- d[is.finite(d$x) & is.finite(d$y), ]
    if (nrow(d) < 10 || length(unique(d$x)) < 4) return(rep(NA_real_, length(grid)))
    fit <- tryCatch(stats::loess(y ~ x, d, span = span), error = function(e) NULL)
    if (is.null(fit)) return(rep(NA_real_, length(grid)))
    suppressWarnings(stats::predict(fit, newdata = data.frame(x = grid)))
  }, numeric(length(grid)))
  data.frame(x = grid, y = rowMeans(preds, na.rm = TRUE))
}



# Internal: residuals vs. predicted plot (pooled) ==============================
#
# Intentionally plots RAW residuals (sigma = 1), unlike the vs-predictor and
# index plots, which standardize by sigma_i. This is a deliberate design choice
# -- leave it on the raw scale; do not "fix" it to match the standardized panels.

#' @keywords internal
.residuals_vs_predicted <- function(imp_list, dv, style) {
  n     <- nrow(imp_list[[1]])
  xfun  <- function(d) d[[paste0(dv, ".predicted")]]
  pts   <- .pooled_xy(imp_list, dv, xfun, sigma = rep(1, n))
  curve <- .pooled_loess_curve(imp_list, dv, xfun, sigma = rep(1, n))
  a     <- .pooled_alpha(style$point_alpha, length(imp_list))

  ggplot2::ggplot(pts, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = "grey50",
      linewidth  = style$line_width * 0.5
    ) +
    ggplot2::geom_point(
      color = style$point_color,
      alpha = a,
      size  = style$point_size
    ) +
    ggplot2::geom_line(
      data      = curve,
      ggplot2::aes(x = x, y = y),
      color     = style$curve_color,
      linewidth = style$line_width,
      na.rm     = TRUE
    ) +
    ggplot2::labs(
      title = sprintf("Residual vs. Predicted: %s", dv),
      x     = "Predicted",
      y     = "Raw Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: standardized residual vs. predictor plot ===========================
#
# Plots standardized residuals (residual / sigma_i; see `.pooled_sigma`)
# against a predictor. Always-standardized so the y-axis is on the same
# (SD-unit) scale as the standardized residual index plot, with matching
# +/- index_thresholds reference bands. Under a homoscedastic fit the
# standardization is by a single scalar sd, so the cloud shape is identical
# to a raw-residual plot; under a heteroscedastic fit each residual is
# divided by its own model-implied sigma_i, so a successfully modeled funnel
# should collapse to a flat band.

#' @keywords internal
.residuals_vs_predictor <- function(imp_list, dv, predictor, is_discrete,
                                    index_thresholds, style, center_col = NULL) {
  pcol <- .resolve_predictor_col(imp_list[[1]], predictor)
  if (is.na(pcol)) return(NULL)

  # Group-mean centering (level-1 only). When the predictor is centered at its
  # latent group mean (CENTER: groupmean = ...), the value entering the model is
  # the within-cluster deviation `pcol - center_col`, not the raw score, so plot
  # that. `center_col` is the `<pred>.mean[clusterid]` column, passed by the
  # caller ONLY for groupmean-listed predictors; NULL (or an absent column)
  # leaves the raw predictor and label untouched.
  centered <- !is.null(center_col) && center_col %in% names(imp_list[[1]])
  xfun  <- if (centered) function(d) d[[pcol]] - d[[center_col]] else function(d) d[[pcol]]
  xlab  <- if (centered) paste0(predictor, " (group-mean centered)") else predictor

  sigma <- .pooled_sigma(imp_list, dv)
  df    <- .pooled_xy(imp_list, dv, xfun, sigma)        # stacked across imputations
  a     <- .pooled_alpha(style$point_alpha, length(imp_list))

  # A predictor modeled as continuous but with few distinct values is likely a
  # categorical variable not declared ORDINAL/NOMINAL. Two thresholds apply:
  # `few_values_warn` (distinct values at or below which we warn about possible
  # miscoding; 0 disables), and a fixed count (10) at or below which we render
  # the panel as discrete (category means) instead of fitting a loess, since a
  # loess is unstable or uninformative over so few x-values.
  nux      <- length(unique(xfun(imp_list[[1]])))
  few_vals <- nux <= 10L
  if (!is_discrete && style$few_values_warn > 0 && nux <= style$few_values_warn) {
    warning(sprintf(
      paste0("Predictor '%s' is modeled as continuous but has only %d distinct ",
             "value(s). If it is categorical, declare it as ORDINAL or NOMINAL ",
             "in the model and re-fit. (The plot displays category means.)"),
      predictor, nux), call. = FALSE)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = "grey50",
      linewidth  = style$line_width * 0.5
    )

  if (length(index_thresholds)) {
    p <- p + ggplot2::geom_hline(
      yintercept = index_thresholds,
      color      = "grey50",
      linetype   = "dashed",
      linewidth  = style$line_width * 0.5
    )
  }

  if (is_discrete || few_vals) {
    p <- p +
      ggplot2::geom_point(
        color = style$point_color,
        alpha = a,
        size  = style$point_size
      ) +
      ggplot2::stat_summary(
        ggplot2::aes(x = round(x)),
        fun   = mean,
        geom  = "point",
        color = style$curve_color,
        size  = 3,
        shape = 18
      ) +
      ggplot2::scale_x_continuous(breaks = .integer_breaks)
  } else {
    curve <- .pooled_loess_curve(imp_list, dv, xfun, sigma)
    p <- p +
      ggplot2::geom_point(
        color = style$point_color,
        alpha = a,
        size  = style$point_size
      ) +
      ggplot2::geom_line(
        data      = curve,
        ggplot2::aes(x = x, y = y),
        color     = style$curve_color,
        linewidth = style$line_width,
        na.rm     = TRUE
      )
  }

  p +
    ggplot2::labs(
      title = sprintf("Standardized Residual vs. Predictor: %s ~ %s",
                      dv, predictor),
      x     = xlab,
      y     = "Standardized Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: probability-scale binned residuals plot (pooled) ===================
#
# Implements Gelman and Hill's (2007) binned-residuals diagnostic for a
# NOMINAL outcome category, computed within each imputed data set and pooled:
#
#   1. Within each imputation, compute raw residuals on the probability scale,
#         r_i = I(Y_i = k) - p_hat_i,
#      where p_hat_i is the predicted P(Y_i = k) (read from that imputation's
#      "<parent>.<k>.probability" column) and I() is the category indicator on
#      that imputation's parent variable (modal/observed code).
#   2. Fix bin edges once from the pooled predicted probabilities (quantiles, so
#      bins are comparable across imputations), assign each imputation's cases to
#      those bins, and compute each imputation's bin-mean predicted probability
#      and bin-mean residual.
#   3. Average the bin means across imputations (point estimate; no error bands).
#   4. Overlay a fit-then-pool loess: fit on each imputation's case-level cloud,
#      predict on a common grid, average the curves. Reference line at y = 0.
#
# Multilevel validity: p_hat_i is Blimp's per-row predicted probability, which
# for a multilevel model is the CONDITIONAL (cluster-specific) probability -- the
# random intercept is already folded in. r_i = I(Y_i = k) - p_hat_i is therefore
# a conditional residual, so binning it is a legitimate conditional calibration /
# functional-form check: E[r | p_hat] (this plot) and E[r | x] (the vs-predictor
# variant) are unbiased regardless of clustering. Gelman and Hill's +/-2 SE bands
# are deliberately NOT drawn -- they assume independent within-bin Bernoullis and
# would be anticonservative under positive within-cluster correlation. Clustering
# inflates the variance of each bin mean but not its expectation, and no
# inferential claim is made on the bin means, so the point-estimate curve holds.
#
# Residuals stay on the raw probability metric (no standardization). Returns NULL
# when there are too few cases (< 16) or the parent / probability columns are
# missing.

#' @keywords internal
.binned_residuals <- function(imp_list, parent, category, style) {
  pred_col <- paste0(parent, ".", category, ".probability")
  if (!pred_col %in% names(imp_list[[1]])) return(NULL)
  if (!parent   %in% names(imp_list[[1]])) return(NULL)

  # Per-imputation case-level (predicted probability, probability-scale residual)
  xy_list <- lapply(imp_list, function(d) {
    p  <- d[[pred_col]]
    ok <- as.numeric(d[[parent]] == category)
    z  <- data.frame(x = p, y = ok - p)
    z[is.finite(z$x) & is.finite(z$y), ]
  })

  n_keep <- nrow(xy_list[[1]])
  if (n_keep < 16) return(NULL)

  # Fixed bin edges from the pooled predicted probabilities so the bins line up
  # across imputations; average the per-imputation bin means at stable x.
  allp   <- unlist(lapply(xy_list, function(d) d$x), use.names = FALSE)
  n_bins <- max(1L, floor(sqrt(n_keep)))
  edges  <- unique(stats::quantile(allp, probs = seq(0, 1, length.out = n_bins + 1),
                                   na.rm = TRUE))
  if (length(edges) < 3) return(NULL)

  per_imp <- lapply(xy_list, function(d) {
    b  <- cut(d$x, breaks = edges, include.lowest = TRUE, labels = FALSE)
    pm <- tapply(d$x, b, mean)
    rm <- tapply(d$y, b, mean)
    data.frame(bin = as.integer(names(pm)),
               pred_mean = as.numeric(pm), resid_mean = as.numeric(rm))
  })
  allbins <- do.call(rbind, per_imp)
  bin_summary <- do.call(rbind, lapply(split(allbins, allbins$bin), function(z)
    data.frame(pred_mean  = mean(z$pred_mean,  na.rm = TRUE),
               resid_mean = mean(z$resid_mean, na.rm = TRUE))))

  # Fit-then-pool loess on the case-level cloud (probability scale)
  curve <- .pooled_loess_xy(xy_list)

  # Title: include category index when parent is multi-category (or always,
  # for consistency with how nominal columns are named)
  title_dv <- if (is.na(category)) parent else
    sprintf("%s.%d", parent, as.integer(category))

  ggplot2::ggplot(bin_summary, ggplot2::aes(x = pred_mean, y = resid_mean)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = "grey50",
      linewidth  = style$line_width * 0.5
    ) +
    ggplot2::geom_point(
      color = style$point_color,
      alpha = style$point_alpha,
      size  = style$point_size * 1.5
    ) +
    ggplot2::geom_line(
      data      = curve,
      ggplot2::aes(x = x, y = y),
      color     = style$curve_color,
      linewidth = style$line_width,
      na.rm     = TRUE
    ) +
    ggplot2::labs(
      title = sprintf("Binned Residual vs. Predicted: %s", title_dv),
      x     = "Mean Predicted Probability",
      y     = "Mean Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: binned residuals vs. each predictor (pooled) =======================
#
# Probability-scale analogue of .residuals_vs_predictor for NOMINAL outcomes,
# computed within each imputation and pooled. For each predictor, residuals
# r_i = I(Y_i = k) - p_hat_i are formed within each imputation and binned by the
# predictor value. See .binned_residuals for why this stays valid under a
# multilevel model: p_hat is the conditional (cluster-specific) probability, so
# r_i is a conditional residual and E[r | x] is unbiased; the +/-2 SE bands that
# clustering would invalidate are not drawn.
#
# Continuous predictors: fixed bin edges from the pooled predictor (quantiles,
# floor(sqrt(n)) bins) keep bins comparable across imputations; each imputation's
# bin means are averaged (point estimate), and a fit-then-pool loess is overlaid
# (fit per imputation on the case-level cloud, curves averaged). Discrete
# predictors (declared ORDINAL, NOMINAL, or COUNT) get one mean residual per
# observed level, averaged across imputations. No error bands; residuals stay on
# the raw probability metric.
#
# Systematic curvature against a continuous predictor (or a sloped pattern across
# levels of a discrete one) indicates that the predictor is being modeled with
# the wrong functional form -- e.g., a quadratic term should be added, or the
# variable should enter on a transformed scale.
#
# Returns NULL when there are too few cases (< 16) for a meaningful binned plot,
# or when any required column is missing.

#' @keywords internal
.binned_residuals_vs_predictor <- function(imp_list, parent, category, predictor,
                                           is_discrete, style) {
  pred_col <- paste0(parent, ".", category, ".probability")
  pcol     <- .resolve_predictor_col(imp_list[[1]], predictor)
  if (!pred_col %in% names(imp_list[[1]])) return(NULL)
  if (!parent   %in% names(imp_list[[1]])) return(NULL)
  if (is.na(pcol))                         return(NULL)

  # Per-imputation case-level (predictor, probability-scale residual)
  xy_list <- lapply(imp_list, function(d) {
    p  <- d[[pred_col]]
    ok <- as.numeric(d[[parent]] == category)
    z  <- data.frame(x = d[[pcol]], y = ok - p)
    z[is.finite(z$x) & is.finite(z$y), ]
  })

  n_keep   <- nrow(xy_list[[1]])
  if (n_keep < 16) return(NULL)
  title_dv <- sprintf("%s.%d", parent, as.integer(category))

  if (is_discrete) {
    # One mean residual per observed level, averaged across imputations.
    per_imp <- lapply(xy_list, function(d) {
      rm <- tapply(d$y, d$x, mean)
      data.frame(x_mean = as.numeric(names(rm)), resid_mean = as.numeric(rm))
    })
    alll <- do.call(rbind, per_imp)
    bin_summary <- do.call(rbind, lapply(split(alll, alll$x_mean), function(z)
      data.frame(x_mean = z$x_mean[1], resid_mean = mean(z$resid_mean, na.rm = TRUE))))
    if (nrow(bin_summary) < 2) return(NULL)

    p <- ggplot2::ggplot(bin_summary,
                         ggplot2::aes(x = x_mean, y = resid_mean)) +
      ggplot2::geom_hline(
        yintercept = 0,
        color      = "grey50",
        linewidth  = style$line_width * 0.5
      ) +
      ggplot2::geom_point(
        color = style$point_color,
        alpha = style$point_alpha,
        size  = style$point_size * 1.5
      ) +
      ggplot2::scale_x_continuous(breaks = .integer_breaks)

  } else {
    # Continuous predictor: fixed bins on the pooled predictor, averaged
    # per-imputation bin means; fit-then-pool loess on the case-level cloud.
    allx   <- unlist(lapply(xy_list, function(d) d$x), use.names = FALSE)
    n_bins <- max(1L, floor(sqrt(n_keep)))
    edges  <- unique(stats::quantile(allx, probs = seq(0, 1, length.out = n_bins + 1),
                                     na.rm = TRUE))
    if (length(edges) < 3) return(NULL)

    per_imp <- lapply(xy_list, function(d) {
      b  <- cut(d$x, breaks = edges, include.lowest = TRUE, labels = FALSE)
      xm <- tapply(d$x, b, mean)
      rm <- tapply(d$y, b, mean)
      data.frame(bin = as.integer(names(xm)),
                 x_mean = as.numeric(xm), resid_mean = as.numeric(rm))
    })
    allbins <- do.call(rbind, per_imp)
    bin_summary <- do.call(rbind, lapply(split(allbins, allbins$bin), function(z)
      data.frame(x_mean     = mean(z$x_mean,     na.rm = TRUE),
                 resid_mean = mean(z$resid_mean, na.rm = TRUE))))
    curve <- .pooled_loess_xy(xy_list)

    p <- ggplot2::ggplot(bin_summary,
                         ggplot2::aes(x = x_mean, y = resid_mean)) +
      ggplot2::geom_hline(
        yintercept = 0,
        color      = "grey50",
        linewidth  = style$line_width * 0.5
      ) +
      ggplot2::geom_point(
        color = style$point_color,
        alpha = style$point_alpha,
        size  = style$point_size * 1.5
      ) +
      ggplot2::geom_line(
        data      = curve,
        ggplot2::aes(x = x, y = y),
        color     = style$curve_color,
        linewidth = style$line_width,
        na.rm     = TRUE
      )
  }

  p +
    ggplot2::labs(
      title = sprintf("Binned Residual vs. Predictor: %s ~ %s", title_dv, predictor),
      x     = predictor,
      y     = "Mean Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: predicted probability vs. predictor (overlay) ======================
#
# Builds a single plot per predictor that overlays the predicted probabilities
# for every non-reference category (NOMINAL outcomes) or every category
# (ORDINAL outcomes). Each category is rendered with a matched color, point
# shape, and line type so the legend can be read in either color or grayscale
# reproduction. The plot answers the question "how does P(Y = k) vary across
# the predictor, for each k?" -- conceptually the same object as a category
# response curve in IRT.
#
# All M imputations are stacked: the predicted probabilities are per-imputation
# quantities (they vary across MCMC draws), so every draw is shown, matching the
# other residuals_plot panels rather than collapsing to the averaged data set.
# Continuous predictors get the stacked case-level scatter plus a per-category
# FIT-THEN-POOL loess (fit within each imputation, the M curves averaged, via
# `.pooled_loess_xy`) -- NOT a geom_smooth over the stacked cloud. Discrete
# predictors get one mean point per (category, level), computed within each
# imputation and averaged across M, connected by per-category lines.
#
# The function discovers categories from the `<parent>.<k>.probability`
# columns present in the imputations; NOMINAL outcomes contribute K - 1
# probability columns and ORDINAL outcomes contribute K, and the plot is built
# from whatever is available.
#
# Returns NULL if no probability columns are found or the predictor is missing
# from the imputations.

#' @keywords internal
.predicted_probability_vs_predictor <- function(imp_list, parent, predictor,
                                                is_discrete, style) {
  d1   <- imp_list[[1]]
  pcol <- .resolve_predictor_col(d1, predictor)
  if (is.na(pcol)) return(NULL)

  # Detect <parent>.<k>.probability columns
  prob_rx   <- paste0("^", parent, "\\.([0-9]+)\\.probability$")
  prob_cols <- grep(prob_rx, names(d1), value = TRUE)
  if (!length(prob_cols)) return(NULL)

  # Category indices, sorted
  k_levels  <- as.integer(sub(prob_rx, "\\1", prob_cols))
  k_order   <- order(k_levels)
  prob_cols <- prob_cols[k_order]
  k_levels  <- k_levels[k_order]
  K         <- length(prob_cols)
  cats      <- as.character(k_levels)

  # Long-format scatter: one row per case per category, STACKED across all M
  # imputations (predicted probabilities are per-imputation quantities, so every
  # draw is shown rather than collapsing to the averaged data set).
  long_df <- do.call(rbind, lapply(imp_list, function(d) {
    do.call(rbind, lapply(seq_len(K), function(i) {
      z <- data.frame(
        x        = d[[pcol]],
        prob     = d[[prob_cols[i]]],
        category = factor(cats[i], levels = cats)
      )
      z[is.finite(z$x) & is.finite(z$prob), ]
    }))
  }))
  if (!nrow(long_df)) return(NULL)

  M <- length(imp_list)
  a <- .pooled_alpha(style$point_alpha, M)   # lighten the stacked cloud by M

  # A binary outcome exposes a single probability column (K == 1, e.g. only
  # P(turnover = 1)); the complementary category is redundant. With one category
  # the color/shape/linetype-by-category encoding carries no contrast, and
  # ggplot's default hue palette would paint that lone series red -- so draw the
  # house point/curve colors with no legend, matching the blue points used
  # elsewhere in residuals_plot.
  single   <- K == 1L
  title_dv <- parent

  # Shape/line-type values for K > 1: redundant differentiation so the legend
  # stays readable after grayscale conversion. Extended to seven; collapses
  # cleanly if K < 7.
  shape_values    <- c(16, 17, 15, 18, 4, 8, 3)[seq_len(K)]
  linetype_values <- c("solid", "dashed", "dotted", "dotdash",
                       "longdash", "twodash", "1F")[seq_len(K)]

  # Stacked point cloud. House blue for a single category; else category hue +
  # shape (shape carries the legend into grayscale).
  pts_layer <- if (single) {
    ggplot2::geom_point(data = long_df, ggplot2::aes(x = x, y = prob),
                        color = style$point_color, alpha = a, size = style$point_size)
  } else {
    ggplot2::geom_point(
      data = long_df,
      ggplot2::aes(x = x, y = prob, color = category, shape = category),
      alpha = a, size = style$point_size)
  }
  p <- ggplot2::ggplot() + pts_layer

  if (is_discrete) {
    # Discrete predictor: per-(category, level) mean probability, computed WITHIN
    # each imputation then averaged across M (matching the binned-vs-predictor
    # discrete convention), connected by per-category lines with larger points.
    cm <- do.call(rbind, lapply(imp_list, function(d) {
      do.call(rbind, lapply(seq_len(K), function(i) {
        z <- data.frame(x = d[[pcol]], prob = d[[prob_cols[i]]],
                        category = factor(cats[i], levels = cats))
        z <- z[is.finite(z$x) & is.finite(z$prob), ]
        if (!nrow(z)) return(NULL)
        stats::aggregate(prob ~ x + category, data = z, FUN = mean)
      }))
    }))
    cell_means <- stats::aggregate(prob ~ x + category, data = cm, FUN = mean)

    if (single) {
      p <- p +
        ggplot2::geom_line(data = cell_means, ggplot2::aes(x = x, y = prob),
                           color = style$curve_color, linewidth = style$line_width) +
        ggplot2::geom_point(data = cell_means, ggplot2::aes(x = x, y = prob),
                            color = style$curve_color, size = style$point_size * 2.5)
    } else {
      p <- p +
        ggplot2::geom_line(
          data = cell_means,
          ggplot2::aes(x = x, y = prob, color = category, linetype = category),
          linewidth = style$line_width) +
        ggplot2::geom_point(
          data = cell_means,
          ggplot2::aes(x = x, y = prob, color = category, shape = category),
          size = style$point_size * 2.5)
    }
    p <- p + ggplot2::scale_x_continuous(breaks = .integer_breaks)
  } else {
    # Continuous predictor: per-category FIT-THEN-POOL loess (fit within each
    # imputation, average the M curves via `.pooled_loess_xy`), drawn as a line
    # -- the pooled-smoother convention, NOT a geom_smooth over the stacked cloud.
    curve_df <- do.call(rbind, lapply(seq_len(K), function(i) {
      xy_list_k <- lapply(imp_list, function(d) {
        z <- data.frame(x = d[[pcol]], y = d[[prob_cols[i]]])
        z[is.finite(z$x) & is.finite(z$y), ]
      })
      cv <- .pooled_loess_xy(xy_list_k)
      if (!nrow(cv)) return(NULL)
      cv$category <- factor(cats[i], levels = cats)
      cv
    }))

    if (!is.null(curve_df) && nrow(curve_df)) {
      if (single) {
        p <- p + ggplot2::geom_line(
          data = curve_df, ggplot2::aes(x = x, y = y),
          color = style$curve_color, linewidth = style$line_width)
      } else {
        p <- p + ggplot2::geom_line(
          data = curve_df,
          ggplot2::aes(x = x, y = y, color = category, linetype = category),
          linewidth = style$line_width)
      }
    }
  }

  p <- p +
    ggplot2::labs(
      title = sprintf("Predicted Probability vs. Predictor: %s ~ %s",
                      title_dv, predictor),
      x     = predictor,
      y     = "Predicted Probability"
    ) +
    .residuals_plot_theme(style$font_size)

  if (single) {
    # One series: suppress the (empty) category legend entirely.
    p + ggplot2::theme(legend.position = "none")
  } else {
    p +
      ggplot2::scale_shape_manual(values = shape_values) +
      ggplot2::scale_linetype_manual(values = linetype_values) +
      ggplot2::labs(
        color    = paste(title_dv, "Category"),
        shape    = paste(title_dv, "Category"),
        linetype = paste(title_dv, "Category")
      ) +
      ggplot2::theme(
        legend.position = "top",
        legend.text     = ggplot2::element_text(size = style$font_size - 2),
        legend.title    = ggplot2::element_text(size = style$font_size - 2)
      )
  }
}



# Internal: standardized residual index plot ===================================
#
# Standardizes each residual by its model-implied sigma_i (per-observation when
# a variance model is fit, scalar marginal sd otherwise; see `.pooled_sigma`).
# Reference lines at +/- index_thresholds; cases with |z| beyond the largest
# absolute threshold get row-number labels.

#' @keywords internal
.residuals_index <- function(imp_list, dv, index_thresholds, style) {
  n     <- nrow(imp_list[[1]])
  sigma <- .pooled_sigma(imp_list, dv)
  rc    <- paste0(dv, ".residual")

  # All M standardized residuals per case (vertical strips), not averaged
  df <- do.call(rbind, lapply(imp_list, function(d)
    data.frame(case = seq_len(n), z = d[[rc]] / sigma)))
  df <- df[is.finite(df$z), ]

  if (nrow(df) < 2) {
    stop("Cannot standardize residuals: fewer than 2 finite values for ", dv)
  }

  a <- .pooled_alpha(style$point_alpha, length(imp_list))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = case, y = z)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = "grey50",
      linewidth  = style$line_width * 0.5
    )

  if (length(index_thresholds)) {
    p <- p + ggplot2::geom_hline(
      yintercept = index_thresholds,
      color      = "grey50",
      linetype   = "dashed",
      linewidth  = style$line_width * 0.5
    )
  }

  p <- p +
    ggplot2::geom_point(
      color = style$point_color,
      alpha = a,
      size  = style$point_size
    )

  # Label cases whose per-case MEAN |z| exceeds the largest threshold
  label_threshold <- if (length(index_thresholds)) max(abs(index_thresholds)) else NA
  if (!is.na(label_threshold)) {
    cm   <- tapply(df$z, df$case, mean)
    dlab <- data.frame(case = as.integer(names(cm)), z = as.numeric(cm))
    p <- .add_outlier_labels(p, dlab, "z", label_threshold, style, two_sided = TRUE)
  }

  # Integer y-axis breaks across the data range, unioned with thresholds
  y_breaks <- function(limits) {
    sort(unique(c(seq(floor(limits[1]), ceiling(limits[2])), index_thresholds)))
  }

  p +
    ggplot2::scale_y_continuous(breaks = y_breaks) +
    ggplot2::labs(
      title = sprintf("Standardized Residual Index: %s", dv),
      x     = "Record Number",
      y     = "Standardized Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: level-2 standardized residual index plot ===========================
#
# The level-2 analogue of .residuals_index. Within each imputation the residual
# is collapsed to one value per cluster (keyed on the bracketed cluster-ID
# column) and standardized by the spread of the cluster residuals; the M
# standardized values per cluster are stacked as vertical strips, mirroring the
# level-1 index plot. Reference lines at +/- index_thresholds; clusters whose
# per-cluster mean |z| exceeds the largest absolute threshold are labeled by
# cluster ID. Returns NULL if the cluster column is missing or fewer than two
# clusters have finite residuals.

#' @keywords internal
.level2_residuals_index <- function(imp_list, var, index_thresholds, style,
                                    cluster_col = .cluster_id_of(var),
                                    label = .distribution_plot_xlab(var)) {
  # `var` is the level-2 residual column (the bracketed random-intercept column
  # for an auto-generated random effect, or `<name>.residual` for a named latent
  # random effect). `cluster_col` and `label` are derived from the bracketed name
  # by default, but are passed explicitly when the residual column is not itself
  # bracketed (a named-latent random effect such as `b0j.residual`, clustered by
  # the model's `clusterid`).
  cl <- cluster_col
  if (!cl %in% names(imp_list[[1]]) || !var %in% names(imp_list[[1]])) return(NULL)

  # One standardized residual per cluster per imputation. Cluster order follows
  # first appearance, which is identical across imputations (shared row order),
  # so cluster index i refers to the same cluster in every imputation.
  df <- do.call(rbind, lapply(imp_list, function(d) {
    g    <- d[[cl]]
    keep <- !duplicated(g)
    ids  <- g[keep]
    r    <- d[[var]][keep]
    ok   <- is.finite(r)
    r    <- r[ok]; ids <- ids[ok]
    if (length(r) < 2) return(NULL)
    s <- stats::sd(r)
    if (!is.finite(s) || s == 0) return(NULL)
    data.frame(case = ids, cluster = seq_along(r), z = (r - mean(r)) / s)
  }))

  if (is.null(df) || nrow(df) < 2) return(NULL)

  a <- .pooled_alpha(style$point_alpha, length(imp_list))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = cluster, y = z)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey50",
                        linewidth = style$line_width * 0.5)

  if (length(index_thresholds)) {
    p <- p + ggplot2::geom_hline(yintercept = index_thresholds, color = "grey50",
                                 linetype = "dashed",
                                 linewidth = style$line_width * 0.5)
  }

  p <- p + ggplot2::geom_point(color = style$point_color, alpha = a,
                               size = style$point_size)

  # Label clusters whose per-cluster mean |z| exceeds the largest threshold,
  # using the cluster ID (via the `case` column expected by .add_outlier_labels)
  # rather than an anonymous row number.
  label_threshold <- if (length(index_thresholds)) max(abs(index_thresholds)) else NA
  if (!is.na(label_threshold)) {
    cm   <- tapply(df$z, df$cluster, mean)
    idm  <- tapply(df$case, df$cluster, function(v) v[1])
    dlab <- data.frame(
      cluster = as.integer(names(cm)),
      z       = as.numeric(cm),
      case    = idm[names(cm)]
    )
    p <- .add_outlier_labels(p, dlab, "z", label_threshold, style, two_sided = TRUE)
  }

  y_breaks <- function(limits) {
    sort(unique(c(seq(floor(limits[1]), ceiling(limits[2])), index_thresholds)))
  }

  p +
    ggplot2::scale_y_continuous(breaks = y_breaks) +
    ggplot2::labs(
      title = sprintf("Standardized Residual Index: %s", label),
      x     = "Cluster Number",
      y     = "Standardized Level-2 Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: is a name the DV's level-2 residual? ===============================
#
# TRUE when `nm` is a random-intercept residual of a plotted DV: it is bracketed
# (`<base>[cluster]`), it is not a random *slope* residual (`<dv>$<x>[cluster]`,
# not diagnosed, via the `$` guard), and its base carries the level-1 columns
# that mark `base` as a DV (`base.residual` plus `base.predicted` or
# `base.probability`). This base-is-a-DV test is what separates the DV's level-2
# residual (`probsolve[school]`, base `probsolve` is a DV) from a latent group
# mean (`stanmath.mean[school]`, base `stanmath.mean` is not a DV) -- Blimp
# writes the bare bracketed column for the level-2 residual and never a
# `[cluster].residual`/`.predicted` companion, so detection keys on the base's
# level-1 columns, not on any suffix of the bracketed name itself.
#
# `cols` is the vector of data column names to test against (typically
# `names(imp_list[[1]])`).

#' @keywords internal
.is_dv_level2_residual <- function(nm, cols) {
  if (!.is_level2_var(nm)) return(FALSE)
  if (grepl("\\$", nm))    return(FALSE)   # random-slope residual: not diagnosed
  base <- sub("\\[.+\\]$", "", nm)
  paste0(base, ".residual") %in% cols &&
    (paste0(base, ".predicted") %in% cols || paste0(base, ".probability") %in% cols)
}



# Internal: resolve a DV's level-2 predictor columns ===========================
#
# Returns the data-column names of the DV's *level-2* predictors -- the ones the
# random intercept can meaningfully be plotted against. A predictor is level-2
# iff it satisfies BOTH:
#   (i)  it appears on the DV's MODEL line (via `.residuals_plot_predictors`),
#        which excludes the non-predictor `.mean[cluster]` columns Blimp
#        auto-generates; and
#   (ii) it is constant within every cluster (zero within-cluster variance),
#        which is what actually makes a predictor level-2.
# Test (ii) catches both latent group means and raw cluster-level covariates,
# so both are returned; genuinely level-1 predictors (varying within cluster)
# are dropped.
#
# Resolution note: a MODEL-line mean term is written `x.mean` but stored as
# `x.mean[cluster]`, so each predictor token is resolved to whichever of
# `<tok>` or `<tok>[cluster]` exists in the data.

# Internal: is a column constant within every cluster? ========================
#
# TRUE when `x` has (numerically) zero within-cluster variance for every level
# of the grouping `g` -- i.e. `x` is a level-2 quantity. Shared by the level-2
# predictor detection and by the level-1 loop's exclusion of higher-level
# predictors, so both use one definition of "constant within cluster".

#' @keywords internal
.is_cluster_constant <- function(x, g) {
  if (!is.numeric(x)) return(FALSE)
  sp <- split(x, g)
  all(vapply(sp, function(v) {
    v <- v[is.finite(v)]
    length(v) == 0L || diff(range(v)) < 1e-8
  }, logical(1)))
}



#' @keywords internal
.level2_predictor_cols <- function(data, model, dv, cluster_col) {
  preds <- tryCatch(.residuals_plot_predictors(model, dv)[[dv]],
                    error = function(e) NULL)
  if (!length(preds)) return(character(0))
  g <- data[[cluster_col]]
  if (is.null(g)) return(character(0))

  out <- character(0)
  for (tok in unique(preds)) {
    # Resolve the model token to a data column (bare, or bracketed mean form).
    col <- if (tok %in% names(data)) {
      tok
    } else if (paste0(tok, "[", cluster_col, "]") %in% names(data)) {
      paste0(tok, "[", cluster_col, "]")
    } else {
      NA_character_
    }
    if (is.na(col)) next

    # Level-2 iff constant within every cluster.
    if (.is_cluster_constant(data[[col]], g)) out <- c(out, col)
  }
  unique(out)
}



# Internal: level-2 residual vs. a level-2 predictor ===========================
#
# The DV's level-2 residual (y) against a cluster-level predictor (x), with ALL
# M imputations stacked -- one (x, y) pair per cluster per imputation (both are
# constant within a cluster within an imputation), matching the all-imputations
# density of the other diagnostics rather than averaging to one point per
# cluster, which would restrict the variation. A trend flags the random
# intercept co-varying with the covariate (an omitted contextual effect /
# departure from independence of random effects). The loess-vs-category-means
# decision uses the SAME rule as the
# level-1 `.residuals_vs_predictor`: category-mean diamonds when the predictor
# is declared discrete OR has <= 10 distinct values (a loess is unstable over so
# few distinct x); otherwise a FIT-THEN-POOL loess (fit within each imputation,
# average the M curves, via `.pooled_loess_xy`) drawn as a line -- the same
# convention as the level-1 plots, NOT a single geom_smooth over the stacked
# cloud. NB: this <= 10 loess cutoff is deliberately higher than the <= 5
# `discrete_max_unique` used for the Q-Q/density gate in distribution_plot -- a
# smoother destabilizes sooner than a normal reference misleads. Returns NULL
# with fewer than three usable clusters.

#' @keywords internal
.level2_residual_vs_predictor <- function(imp_list, var, pcol, is_discrete, style,
                                          cluster_col = .cluster_id_of(var),
                                          label = .distribution_plot_xlab(var)) {
  cl <- cluster_col

  # Per-imputation cluster-level (predictor, residual): one row per cluster per
  # imputation (both constant within a cluster within an imputation). Kept as a
  # LIST for the fit-then-pool loess, and stacked for the scatter, so both use
  # every imputation rather than an average.
  xy_list <- lapply(imp_list, function(d) {
    keep <- !duplicated(d[[cl]])
    xy   <- data.frame(x = d[[pcol]][keep], y = d[[var]][keep])
    xy[is.finite(xy$x) & is.finite(xy$y), ]
  })
  df <- do.call(rbind, xy_list)
  if (nrow(df) < 3) return(NULL)

  a <- .pooled_alpha(style$point_alpha, length(imp_list))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey50",
                        linewidth = style$line_width * 0.5) +
    ggplot2::geom_point(color = style$point_color, alpha = a,
                        size = style$point_size)

  # Loess/diamond decision: same <= 10 distinct-x rule as level-1. For a
  # continuous predictor the curve is a FIT-THEN-POOL loess -- fit within each
  # imputation, average the M curves, via `.pooled_loess_xy`, drawn as a line --
  # exactly the convention the level-1 residual-vs-predictor uses, NOT a single
  # geom_smooth over the stacked cloud. For a discrete predictor the diamonds are
  # per-level means, which are linear so pooling coincides with per-imputation
  # averaging.
  if (is_discrete || length(unique(df$x)) <= 10L) {
    md <- stats::aggregate(y ~ x, data = df, FUN = mean)
    p <- p + ggplot2::geom_point(
      data = md, ggplot2::aes(x = x, y = y),
      color = style$curve_color, size = style$point_size * 2.5, shape = 18
    )
  } else {
    curve <- .pooled_loess_xy(xy_list)
    if (nrow(curve) > 0L) {
      p <- p + ggplot2::geom_line(
        data = curve, ggplot2::aes(x = x, y = y),
        color = style$curve_color, linewidth = style$line_width
      )
    }
  }

  p +
    ggplot2::labs(
      title = sprintf("%s vs. %s",
                      label, .distribution_plot_xlab(pcol)),
      x     = .distribution_plot_xlab(pcol),
      y     = label
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: within-cluster residual-variance heterogeneity =====================
#
# Detects whether the level-1 residual variance differs across level-2 clusters
# (the diagnostic that motivates a cluster-specific-variance model). ALL M
# imputations are stacked (one point per observation per imputation, matching
# the density of the level-1 index plot -- NOT averaged to one value per case,
# which would both desync from the index plot and shrink the very spread this
# plot exists to show), standardized by a SINGLE pooled SD, and drawn as a
# vertical strip of points per cluster, with clusters ordered left to right by
# within-cluster residual SD. Heterogeneity reads off as strips whose vertical
# spread grows across the ordering.
#
# STANDARDIZER -- important. The residuals are divided by ONE pooled SD (shared
# across all clusters), NOT by each cluster's own residual SD. Dividing by a
# cluster's own SD would force every cluster to unit variance by construction,
# making the plot flat regardless of the truth -- a tautology. With a single
# shared SD, unit variance *per cluster* is a genuine, falsifiable prediction of
# the homogeneous-variance model, so departures from it are real signal. (This
# is also why there is no "after" companion standardized by cluster-specific
# variance: that plot is flat by near-construction and carries little
# information; whether a cluster-specific-variance model is warranted is a
# model-comparison question, not a residual-plot question.)
#
# Interpret with care for small clusters: a wide strip built from a handful of
# observations is more likely sampling noise than true heterogeneity. Returns
# NULL if the residual or cluster column is absent, or the pooled SD is not
# usable.

#' @keywords internal
.residuals_by_cluster <- function(imp_list, dv, cluster_col, style) {
  rc <- paste0(dv, ".residual")
  if (!rc %in% names(imp_list[[1]]) ||
      !cluster_col %in% names(imp_list[[1]])) return(NULL)

  # Single pooled within-cluster residual SD = mean of the per-imputation
  # residual SDs (the agreed pooled-SD point estimate). Deliberately a single
  # scalar shared across clusters -- NOT .pooled_sigma's case-specific sigma_i,
  # which under a variance model would divide the heterogeneity out -- so
  # per-cluster departures from unit variance stay visible.
  s <- mean(vapply(imp_list, function(d) stats::sd(d[[rc]], na.rm = TRUE),
                   numeric(1)), na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(NULL)

  # Stack ALL M imputations (one point per observation per imputation), grouped
  # by cluster, standardized by the shared pooled SD.
  df <- do.call(rbind, lapply(imp_list, function(d)
    data.frame(cluster = d[[cluster_col]], z = d[[rc]] / s)))
  df <- df[is.finite(df$z) & !is.na(df$cluster), ]
  if (nrow(df) < 2) return(NULL)

  # Order clusters by within-cluster SD (over the stacked draws) and place each
  # at its integer rank on a continuous x-axis. This matches the dot style of the
  # other index plots and makes the ordering visible: since the points shown ARE
  # the values whose spread is being ranked, each cluster's vertical extent grows
  # from left to right.
  spread  <- tapply(df$z, df$cluster, stats::sd, na.rm = TRUE)
  ord     <- names(sort(spread[is.finite(spread)]))
  df$rank <- match(as.character(df$cluster), ord)
  df      <- df[!is.na(df$rank), ]

  a <- .pooled_alpha(style$point_alpha, length(imp_list))

  ggplot2::ggplot(df, ggplot2::aes(x = rank, y = z)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey50",
                        linewidth = style$line_width * 0.5) +
    ggplot2::geom_hline(yintercept = c(-3, -2, -1, 1, 2, 3), color = "grey70",
                        linetype = "dashed", linewidth = style$line_width * 0.4) +
    ggplot2::geom_point(
      color = style$point_color,
      alpha = a,
      size  = style$point_size * 0.7
    ) +
    ggplot2::labs(
      title = sprintf("Within-Cluster Residual Spread: %s", dv),
      x     = "Cluster (ordered by residual SD)",
      y     = "Standardized Level-1 Residual"
    ) +
    # Dashed reference lines at +/-1, +/-2, +/-3 give a height anchor at every
    # x-position, so the vertical spread of each cluster's strip (and any funnel
    # across the ordering) is readable far from the y-axis. Integer y-breaks put
    # the axis labels on the same levels as the reference lines.
    ggplot2::scale_y_continuous(
      breaks = function(limits) seq(floor(limits[1]), ceiling(limits[2]))
    ) +
    .residuals_plot_theme(style$font_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
}



# Internal: Pearson residual index plot (COUNT outcomes, pooled) ===============
#
# Mirrors .residuals_index, but the y-axis is the Pearson residual computed
# within each imputation from that imputation's predicted mean count and the
# model's NB variance-dispersion parameter alpha (stored in @estimates as
# "<dv> Variance Dispersion Parameter", a single pooled estimate reused across
# imputations). Under the (mu, alpha) NB parameterization, Var(Y) = mu +
# alpha * mu^2, so the person-specific SD is sqrt(mu_i + alpha * mu_i^2). mu_i
# varies by case and by imputation; the SD is pooled per case as the average of
# the per-imputation SDs (point estimate, no between term), and
#
#   z_i^(t) = (y_i^(t) - mu_hat_i^(t)) / sbar_i.
#
# All M standardized residuals are shown per case (vertical strips); cases whose
# per-case mean |z| exceeds the largest absolute threshold get row-number labels.
# Recommended diagnostic in Gelman and Hill (2006, Section 6.2, Figure 6.1b).

#' @keywords internal
.pearson_residuals_index <- function(imp_list, dv, alpha, index_thresholds, style) {
  n  <- nrow(imp_list[[1]])
  pc <- paste0(dv, ".predicted")
  if (!pc %in% names(imp_list[[1]]) || !dv %in% names(imp_list[[1]])) {
    stop("Cannot build Pearson index: missing `", dv, "` or `", pc, "`.")
  }

  # Pooled person-specific Pearson SD: average per-imputation sqrt(mu + alpha*mu^2)
  sbar <- rowMeans(vapply(imp_list, function(d) {
    mu <- d[[pc]]; sqrt(mu + alpha * mu^2)
  }, numeric(n)))

  # All M standardized Pearson residuals per case (vertical strips), not averaged
  df <- do.call(rbind, lapply(imp_list, function(d)
    data.frame(case = seq_len(n), z = (d[[dv]] - d[[pc]]) / sbar)))
  df <- df[is.finite(df$z), ]
  a  <- .pooled_alpha(style$point_alpha, length(imp_list))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = case, y = z)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = "grey50",
      linewidth  = style$line_width * 0.5
    )

  if (length(index_thresholds)) {
    p <- p + ggplot2::geom_hline(
      yintercept = index_thresholds,
      color      = "grey50",
      linetype   = "dashed",
      linewidth  = style$line_width * 0.5
    )
  }

  p <- p +
    ggplot2::geom_point(
      color = style$point_color,
      alpha = a,
      size  = style$point_size
    )

  # Label cases whose per-case MEAN |z| exceeds the largest threshold
  label_threshold <- if (length(index_thresholds)) max(abs(index_thresholds)) else NA
  if (!is.na(label_threshold)) {
    cm   <- tapply(df$z, df$case, mean)
    dlab <- data.frame(case = as.integer(names(cm)), z = as.numeric(cm))
    p <- .add_outlier_labels(p, dlab, "z", label_threshold, style, two_sided = TRUE)
  }

  # Integer y-axis breaks across the data range, unioned with thresholds
  y_breaks <- function(limits) {
    sort(unique(c(seq(floor(limits[1]), ceiling(limits[2])), index_thresholds)))
  }

  p +
    ggplot2::scale_y_continuous(breaks = y_breaks) +
    ggplot2::labs(
      title = sprintf("Standardized Pearson Residual Index: %s", dv),
      x     = "Record Number",
      y     = "Standardized Pearson Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: Pearson residual vs. predictor (COUNT outcomes, pooled) ============
#
# Mirrors .residuals_vs_predictor, but plots Pearson residuals against each
# predictor. The residual is formed within each imputation using that
# imputation's predicted mean count and standardized by the pooled
# person-specific NB SD sbar_i = mean_t sqrt(mu_i^(t) + alpha * mu_i^(t)^2).
# All M (x, z) points are stacked. Continuous predictors get a fit-then-pool
# loess (fit per imputation, curves averaged); discrete predictors get a
# curve-colored mean diamond at each integer level (pooled across imputations).
# Recommended diagnostic in Gelman and Hill (2006, Section 6.2).

#' @keywords internal
.pearson_residuals_vs_predictor <- function(imp_list, dv, predictor, alpha,
                                            is_discrete, style) {
  pcol <- .resolve_predictor_col(imp_list[[1]], predictor)
  if (is.na(pcol)) return(NULL)
  n  <- nrow(imp_list[[1]])
  pc <- paste0(dv, ".predicted")

  # Pooled person-specific Pearson SD (point estimate, no between term)
  sbar <- rowMeans(vapply(imp_list, function(d) {
    mu <- d[[pc]]; sqrt(mu + alpha * mu^2)
  }, numeric(n)))

  # Stacked (predictor, Pearson z) across imputations
  xy_list <- lapply(imp_list, function(d)
    data.frame(x = d[[pcol]], y = (d[[dv]] - d[[pc]]) / sbar))
  df <- do.call(rbind, xy_list)
  df <- df[is.finite(df$x) & is.finite(df$y), ]
  a  <- .pooled_alpha(style$point_alpha, length(imp_list))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = "grey50",
      linewidth  = style$line_width * 0.5
    )

  if (is_discrete) {
    p <- p +
      ggplot2::geom_point(
        color = style$point_color,
        alpha = a,
        size  = style$point_size
      ) +
      ggplot2::stat_summary(
        ggplot2::aes(x = round(x)),
        fun   = mean,
        geom  = "point",
        color = style$curve_color,
        size  = 3,
        shape = 18
      ) +
      ggplot2::scale_x_continuous(breaks = .integer_breaks)
  } else {
    curve <- .pooled_loess_xy(xy_list)
    p <- p +
      ggplot2::geom_point(
        color = style$point_color,
        alpha = a,
        size  = style$point_size
      ) +
      ggplot2::geom_line(
        data      = curve,
        ggplot2::aes(x = x, y = y),
        color     = style$curve_color,
        linewidth = style$line_width,
        na.rm     = TRUE
      )
  }

  p +
    ggplot2::labs(
      title = sprintf("Standardized Pearson Residual vs. Predictor: %s ~ %s",
                      dv, predictor),
      x     = predictor,
      y     = "Standardized Pearson Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: resolve a predictor name to its data column ========================
#
# A predictor referenced in the MODEL block may be stored in the averaged data
# under its bare name (observed and ordinal/nominal/count predictors) or, for a
# latent factor, under `<name>.latent`. Blimp imputes latent variables, so the
# `.latent` column holds a real per-case score that behaves like any other
# continuous predictor for residual-vs-predictor, leverage, and Cook's D.
# Returns the resolved column name, or NA_character_ if neither form is present.

#' @keywords internal
.resolve_predictor_col <- function(data, pred) {
  if (pred %in% names(data)) return(pred)
  latent <- paste0(pred, ".latent")
  if (latent %in% names(data)) return(latent)
  NA_character_
}



# Internal: fixed-effects design terms from the fitted estimates ===============
#
# The leverage/Cook's design matrix must be the SAME fixed-effects design Blimp
# fit, so its terms are read from the estimates table rather than re-parsed from
# the model syntax. For DV `dv`, every coefficient row "<dv> ~ <term>" names one
# design column, already expanded by Blimp into the exact terms it estimated:
# nominal-category dummies as "<v>.<k>", interactions as "a*b", polynomials as
# "x^k". The Intercept row (added by the builder) and the "(standardized)" /
# residual-variance / R2 rows are not design columns and are dropped. Returns the
# ordered term labels, or NULL when the object carries no estimates.

#' @keywords internal
.model_design_terms <- function(model, dv) {
  rn <- tryCatch(rownames(model@estimates), error = function(e) NULL)
  if (is.null(rn) || !length(rn)) return(NULL)
  pre  <- paste0(dv, " ~ ")
  rows <- rn[startsWith(rn, pre) & !endsWith(rn, " (standardized)")]
  if (!length(rows)) return(NULL)
  terms <- substring(rows, nchar(pre) + 1L)
  terms[terms != "Intercept"]
}



# Internal: grand-mean centering constants ======================================
#
# Blimp centers the predictors on its `center` line at their GRAND MEAN before
# they enter the model, and reports each one as a "<var> Grand Mean" row in the
# estimates table. A linear term's centering is absorbed by the intercept and
# does not matter for leverage, but a term INSIDE a nonlinear function (e.g.
# `ln(x1 + 100)`) must use the centered value, so the exact constant is needed.
# Returns a named vector `var -> grand mean` for the centered variables that have
# a grand-mean estimate; variables not listed here enter uncentered.

#' @keywords internal
.center_constants <- function(model) {
  sx  <- model@syntax
  cen <- if ((inherits(sx, "blimp_syntax") || is.list(sx)) &&
             "center" %in% names(sx)) sx$center else NULL
  if (is.null(cen) || !nzchar(cen)) return(numeric(0))
  # keep variable tokens, dropping any groupmean=/grandmean= labels and separators
  cen  <- gsub("\\b(group|grand)mean\\s*=", " ", cen)
  cen  <- gsub("[;=]", " ", cen)
  vars <- unique(strsplit(trimws(cen), "\\s+")[[1]])
  vars <- vars[nzchar(vars)]
  rn   <- tryCatch(rownames(model@estimates), error = function(e) NULL)
  out  <- numeric(0)
  for (v in vars) {
    row <- paste0(v, " Grand Mean")
    if (!is.null(rn) && row %in% rn) out[v] <- model@estimates[row, "Estimate"]
  }
  out
}



# Internal: resolve one variable name to a model-scale column ==================
#
# A bare name in a fitted term maps to saved values that enter as the
# OBSERVED/imputed value (verified against Blimp's saved fitted values, not the
# latent propensities): a nominal-category name "<v>.<k>" becomes the observed
# dummy 1(round(<v>) == k); any other name resolves to its saved column (observed
# column if present, else the latent factor score "<name>.latent"). NULL if the
# name has no saved column.

#' @keywords internal
.resolve_atom <- function(nm, data, nominal_vars, center_map = numeric(0)) {
  cat <- regmatches(nm, regexec("^(.+)\\.([0-9]+)$", nm))[[1]]
  if (length(cat) == 3L && cat[2] %in% nominal_vars && cat[2] %in% names(data)) {
    return(as.numeric(round(data[[cat[2]]]) == as.integer(cat[3])))
  }
  col <- .resolve_predictor_col(data, nm)
  if (is.na(col)) return(NULL)
  v <- data[[col]]
  # grand-mean center when Blimp did (needed for nonlinear terms; a no-op for the
  # column space of a linear term, so harmless there)
  if (nm %in% names(center_map)) v <- v - center_map[[nm]]
  v
}



# Internal: one design column for a fitted term ================================
#
# Rebuilds the model-scale values of a single fixed-effects term from an
# imputation's saved columns, by evaluating the term as an expression over those
# columns. Arithmetic (`+ - * ^` and parentheses) and a whitelist of elementwise
# math functions (ln/log/log10/log2/exp/sqrt/abs) are supported, so a plain
# predictor, a nominal dummy, an interaction ("a*b"), a polynomial ("x^2"), an
# inline sum score ("x1+...+x7"), a grouped interaction ("m*(x1+...+x7)") and a
# transformed predictor ("ln(x1 + 100)") all rebuild. Variables that Blimp
# grand-mean centered are centered here too (via `center_map`), which is required
# INSIDE a nonlinear function and a no-op for the column space of a linear term.
#
# An unknown function, a multi-argument call, or an unresolvable name returns
# NULL: the term is un-sourceable, which makes the whole design non-reconstructable
# and suppresses the influence panels. The reconstruction is always confirmed
# against the saved fitted values (see .residuals_lm_diagnostics), so a wrong
# rebuild (e.g. a function whose centering is not reproduced) refuses rather than
# mislead.

#' @keywords internal
.resolve_design_term <- function(term, data, nominal_vars, center_map = numeric(0)) {
  funs <- list(ln = log, log = log, log10 = log10, log2 = log2,
               exp = exp, sqrt = sqrt, abs = abs)
  expr <- tryCatch(parse(text = term)[[1]], error = function(e) NULL)
  if (is.null(expr)) return(NULL)
  ev <- function(e) {
    if (is.numeric(e)) return(as.numeric(e))
    if (is.name(e))    return(.resolve_atom(as.character(e), data, nominal_vars, center_map))
    if (is.call(e)) {
      op <- as.character(e[[1]])
      if (op == "(" && length(e) == 2L) return(ev(e[[2]]))
      if (op == "-" && length(e) == 2L) {                 # unary minus
        a <- ev(e[[2]]); if (is.null(a)) return(NULL); return(-a)
      }
      if (op %in% c("+", "-", "*", "^") && length(e) == 3L) {
        a <- ev(e[[2]]); b <- ev(e[[3]])
        if (is.null(a) || is.null(b)) return(NULL)
        return(switch(op, "+" = a + b, "-" = a - b, "*" = a * b, "^" = a ^ b))
      }
      if (op %in% names(funs) && length(e) == 2L) {        # whitelisted math function
        a <- ev(e[[2]]); if (is.null(a)) return(NULL)
        return(funs[[op]](a))
      }
      return(NULL)                                         # unknown call -> refuse
    }
    NULL
  }
  ev(expr)
}



# Internal: design matrix builder ==============================================
#
# Builds the leverage/Cook's design matrix for one imputation from the fitted
# term list (.model_design_terms): intercept plus one column per term, each
# sourced by .resolve_design_term. Returns NULL if ANY term cannot be rebuilt
# from saved columns, so leverage/Cook's are produced only when the exact fitted
# design is recoverable.

#' @keywords internal
.build_design_matrix <- function(data, terms, nominal_vars, center_map = numeric(0)) {
  cols <- list(Intercept = rep(1, nrow(data)))
  for (term in terms) {
    v <- .resolve_design_term(term, data, nominal_vars, center_map)
    if (is.null(v)) return(NULL)
    cols[[term]] <- v
  }
  do.call(cbind, cols)
}



# Internal: leverage and Cook's distance =======================================
#
# Computes leverage (diagonal of the hat matrix) and Cook's distance WITHIN each
# imputation from that imputation's design matrix and residuals, then averages
# each casewise value across imputations (one pooled point per case). Returns
# NULL if the design matrix is singular in every imputation (which can happen
# with collinear predictors or nominal variables having too few levels).
#
# Leverage h_ii = i-th diagonal of X(X'X)^-1 X' -- a pure function of the design
# matrix, so nothing is pooled before averaging it across imputations.
#
# Cook's D_i = (e_i^2 / (p * sigma_i^2)) * (h_ii / (1 - h_ii)^2). The error
# variance is the one estimated statistical quantity in this formula, so it is
# POOLED FIRST rather than re-estimated per imputation: sigma_i comes from
# `.pooled_sigma` (a single pooled residual SD shared across cases, or the
# per-observation model-implied SD when a variance model is fit) -- the SAME
# benchmark the standardized residual index uses. The residual e_i and leverage
# h_ii remain per-imputation; only the variance scale is shared, so all three
# residual/influence diagnostics standardize by one common sigma.
#
# `terms` is the fitted fixed-effects design (from .model_design_terms). Before
# using a rebuilt X, its column space is checked against Blimp's saved fitted
# values: the saved <dv>.predicted must be reproduced by X (it is a linear
# combination of the true design columns), or the design is missing/mis-sourcing
# a term and NULL is returned -- influence is emitted only when the exact fitted
# design is recovered, never from an approximation.

#' @keywords internal
.residuals_lm_diagnostics <- function(imp_list, dv, terms, nominal_vars, verbose,
                                      center_map = numeric(0)) {
  if (is.null(terms)) {
    if (verbose) message("Skipping leverage/Cook's D for `", dv,
                         "`: no fixed-effects design found in the estimates.")
    return(NULL)
  }
  n   <- nrow(imp_list[[1]])
  M   <- length(imp_list)
  rc  <- paste0(dv, ".residual")
  pc  <- paste0(dv, ".predicted")
  has_pred <- pc %in% names(imp_list[[1]])
  lev <- matrix(NA_real_, n, M)
  ck  <- matrix(NA_real_, n, M)
  p_used <- NA_integer_

  # Single pooled error-variance scale, shared across all imputations (and, when
  # a variance model is present, per observation). Cook's D is scaled by this
  # ONE benchmark rather than each imputation's own SSE/(n - p), so it matches
  # the standardized residual index's standardizer and only the residual and
  # leverage vary across imputations.
  sigma <- .pooled_sigma(imp_list, dv)

  # Compute leverage and Cook's D within each imputation, then average per case.
  for (t in seq_len(M)) {
    d  <- imp_list[[t]]
    X  <- .build_design_matrix(d, terms, nominal_vars, center_map)
    if (is.null(X)) {
      if (verbose) message("Skipping leverage/Cook's D for `", dv,
                           "`: fitted design not reconstructable from saved columns.")
      return(NULL)
    }
    e  <- d[[rc]]
    keep <- stats::complete.cases(X) & is.finite(e)
    Xk <- X[keep, , drop = FALSE]
    ek <- e[keep]
    nn <- length(ek)
    p  <- ncol(Xk)
    if (nn <= p) next

    # Column-space guard: the rebuilt design must reproduce the saved fitted
    # values. If <dv>.predicted is not (numerically) in the span of Xk, a design
    # column is missing or mis-sourced -- emit nothing rather than a wrong plot.
    if (has_pred) {
      y   <- d[[pc]][keep]
      fit <- tryCatch(stats::lm.fit(Xk, y), error = function(err) NULL)
      if (is.null(fit)) next
      if (max(abs(fit$residuals)) > 5e-3 * (stats::sd(y) + 1e-8)) {
        if (verbose) message("Skipping leverage/Cook's D for `", dv,
                             "`: rebuilt design does not reproduce ", pc, ".")
        return(NULL)
      }
    }

    XtX_inv <- tryCatch(solve(crossprod(Xk)), error = function(err) NULL)
    if (is.null(XtX_inv)) next
    h  <- rowSums((Xk %*% XtX_inv) * Xk)
    lev[keep, t] <- h
    ck[keep, t]  <- (ek^2 / (p * sigma[keep]^2)) * (h / (1 - h)^2)
    p_used <- p
  }

  if (is.na(p_used)) {
    if (verbose) {
      message("Skipping leverage/Cook's D for `", dv,
              "`: singular design or too few complete cases in every imputation.")
    }
    return(NULL)
  }

  # Pool per case (rows that were never estimable stay NA -> dropped downstream)
  list(
    leverage = rowMeans(lev, na.rm = TRUE),
    cooks    = rowMeans(ck,  na.rm = TRUE),
    p = p_used, n = n
  )
}



# Internal: leverage index plot ================================================

#' @keywords internal
.residuals_leverage <- function(diag, dv, style) {
  cuts <- c(2 * diag$p / diag$n, 3 * diag$p / diag$n)
  df   <- data.frame(case = seq_along(diag$leverage), leverage = diag$leverage)
  df   <- df[!is.na(df$leverage), ]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = case, y = leverage)) +
    ggplot2::geom_hline(
      yintercept = cuts,
      color      = "grey50",
      linetype   = "dashed",
      linewidth  = style$line_width * 0.5
    ) +
    ggplot2::annotate(
      "text",
      x      = -Inf,
      y      = cuts,
      label  = c("2p/n", "3p/n"),
      hjust  = -0.1,
      vjust  = -0.3,
      color  = "black",
      size   = style$font_size / 2.85,
      family = if (is.null(style$label_family)) "" else style$label_family
    ) +
    ggplot2::geom_point(
      color = style$point_color,
      alpha = style$point_alpha,
      size  = style$point_size
    )

  # Label cases above the upper cutoff (3p/n)
  p <- .add_outlier_labels(p, df, "leverage", cuts[2], style)

  p +
    ggplot2::labs(
      title = sprintf("Leverage: %s", dv),
      x     = "Record Number",
      y     = "Leverage"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: Cook's distance index plot =========================================

#' @keywords internal
.residuals_cooks <- function(diag, dv, style) {
  cut <- 4 / diag$n
  df  <- data.frame(case = seq_along(diag$cooks), cooks = diag$cooks)
  df  <- df[!is.na(df$cooks), ]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = case, y = cooks)) +
    ggplot2::geom_hline(
      yintercept = cut,
      color      = "grey50",
      linetype   = "dashed",
      linewidth  = style$line_width * 0.5
    ) +
    ggplot2::annotate(
      "text",
      x      = -Inf,
      y      = cut,
      label  = "4/n",
      hjust  = -0.1,
      vjust  = -0.3,
      color  = "black",
      size   = style$font_size / 2.85,
      family = if (is.null(style$label_family)) "" else style$label_family
    ) +
    ggplot2::geom_point(
      color = style$point_color,
      alpha = style$point_alpha,
      size  = style$point_size
    )

  # Label cases above the 4/n cutoff
  p <- .add_outlier_labels(p, df, "cooks", cut, style)

  p +
    ggplot2::labs(
      title = sprintf("Cook's Distance: %s", dv),
      x     = "Record Number",
      y     = "Cook's D"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: outlier labels =====================================================
#
# Adds row-number labels to cases exceeding a threshold on a diagnostic plot.
# When more than `style$max_labels` cases exceed the threshold, only the most
# extreme `max_labels` cases (ranked by |value| in the two-sided case, by
# value in the one-sided case) are labeled. This keeps large-sample plots
# legible while still flagging the cases of substantive interest. Uses
# ggrepel::geom_text_repel if the package is installed (cleaner label
# placement); falls back to ggplot2::geom_text otherwise.
#
# Used by .residuals_index, .residuals_leverage, .residuals_cooks, and the
# Pearson-residual variants for COUNT outcomes.

#' @keywords internal
.add_outlier_labels <- function(p, df, value_col, threshold, style, two_sided = FALSE) {
  values <- df[[value_col]]

  outliers <- if (two_sided) {
    df[abs(values) >= abs(threshold), ]
  } else {
    df[values >= threshold, ]
  }

  if (!nrow(outliers)) return(p)

  # Cap at max_labels most extreme cases
  max_labels <- if (is.null(style$max_labels)) Inf else style$max_labels
  if (nrow(outliers) > max_labels) {
    rank_values <- outliers[[value_col]]
    rank_keys   <- if (two_sided) abs(rank_values) else rank_values
    keep_idx    <- order(rank_keys, decreasing = TRUE)[seq_len(max_labels)]
    outliers    <- outliers[keep_idx, ]
  }

  # Roughly 80% of axis text (axis text is font_size / 2.85 in mm)
  label_size   <- style$font_size / 3.5
  label_family <- if (is.null(style$label_family)) "" else style$label_family

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p + ggrepel::geom_text_repel(
      data         = outliers,
      ggplot2::aes(label = case),
      size         = label_size,
      color        = "black",
      family       = label_family,
      max.overlaps = Inf,
      seed         = 42,
      min.segment.length = 0.2
    )
  } else {
    p + ggplot2::geom_text(
      data   = outliers,
      ggplot2::aes(label = case),
      size   = label_size,
      color  = "black",
      family = label_family,
      vjust  = -0.7
    )
  }
}



# Internal: shared theme for residuals plots ===================================

#' @keywords internal
.residuals_plot_theme <- function(font_size) {
  ggplot2::theme_minimal(base_size = font_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(size = font_size),
      axis.text.y      = ggplot2::element_text(size = font_size),
      axis.title.x     = ggplot2::element_text(
                           size   = font_size,
                           margin = ggplot2::margin(t = 12)
                         ),
      axis.title.y     = ggplot2::element_text(size = font_size),
      plot.title       = ggplot2::element_text(
                           hjust = 0.5,
                           face  = "bold",
                           size  = font_size + 2
                         ),
      legend.position  = "none"
    )
}
