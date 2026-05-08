# rblimp plotting functions ====================================================
#
# Cleaned versions of plotting functions from the original `functions.R`,
# prepared for production handoff to the rblimp package programmer.
# Cleanup decisions are documented in `rblimp_cleanup_changelog.md` in this
# same folder.
#
# Functions cleaned so far:
#   - distribution_plot   (supersedes imputation_plot and univariate_plot)
#   - residuals_plot      (streamlined; uses averaged imputations)
#   - .get_discrete_vars  (renamed from .get_categorical_vars; extended to COUNT)
#
# Internal syntax-parsing helpers folded in (formerly in functions.R):
#   .syntax_as_text, .get_model_block, .extract_model_vars,
#   .normalize_bracket_names, .add_bracket_copies
#
# This file is self-contained — no external sourcing required.



# .get_discrete_vars ===========================================================
#
# Returns variable names declared as ORDINAL, NOMINAL, or COUNT in the model
# script. Expands ranges like dep1:dep7 -> dep1, dep2, ..., dep7. Works for
# both modern list syntax ($ordinal, $nominal, $count) and legacy flat text
# with ORDINAL:, NOMINAL:, COUNT: headers.
#
# Used by distribution_plot to force integer-only X-axis ticks on declared
# discrete variables. Reserved as shared infrastructure for other plot
# functions in subsequent cleanup passes.

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
#' @param verbose Logical. If `TRUE`, prints informational messages about
#'   variables being plotted or skipped. Default `FALSE`.
#'
#' @return A named list of class `"distribution_plot"`, with one ggplot
#'   object per variable. Variables that cannot be plotted (e.g., non-numeric
#'   data, no observed or imputed values present) are silently dropped unless
#'   `verbose = TRUE`. Calling the list at the console auto-prints every
#'   plot in turn; assigning the result is silent.
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

  # Build one plot per variable
  plots <- setNames(
    lapply(vars, function(v) {
      .distribution_plot_one(
        var           = v,
        model         = model,
        n_imps        = n_imps,
        force_integer = v %in% discrete_vars,
        bins          = bins,
        style         = style,
        verbose       = verbose
      )
    }),
    vars
  )

  plots <- plots[!vapply(plots, is.null, logical(1))]
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
# the three model-implied suffixes (.residual, .latent, .probability).
# Excludes .predicted.

#' @keywords internal
.resolve_plot_vars <- function(model, vars, verbose) {
  vars_imp1   <- names(model@imputations[[1]])
  vars_varimp <- names(model@variance_imp)
  vars_all    <- intersect(vars_varimp, vars_imp1)

  # Excluded by default; .predicted variables aren't useful as univariate
  # distributions. Users can still request them explicitly via `vars`.
  vars_eligible <- vars_all[!grepl("\\.predicted$", vars_all)]

  if (is.null(vars)) {
    model_vars       <- .extract_model_vars(model)
    model_implied_rx <- "\\.(residual|latent|probability)$"
    model_implied    <- vars_eligible[grepl(model_implied_rx, vars_eligible)]
    raw_present      <- intersect(vars_eligible, model_vars)
    vars             <- unique(c(raw_present, model_implied))

    if (!length(vars)) {
      stop("No matching variables found in MODEL section with imputation data.")
    }
    if (verbose) {
      message("Plotting variables: ", paste(vars, collapse = ", "))
    }
  } else {
    missing_vars <- setdiff(vars, vars_all)
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
  if (grepl("\\.(residual|latent|probability)$", var)) {
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
  vimp <- model@variance_imp[[var]]
  if (is.null(vimp)) return(NULL)

  # Blimp convention: nonzero finite entries flag missing/imputed cases.
  # For .residual / .latent / .probability the variance_imp entries flag
  # iteration-varying cases, so this captures all values that vary across
  # iterations (i.e., the model-implied posterior draws).
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



# Internal: x-axis label helper ================================================
#
# Translates rblimp suffix variables into reader-friendly axis labels:
#   <var>.residual    -> "<var> Residual"
#   <var>.latent      -> "<var> Latent Response"
#   <var>.probability -> "<var> Predicted Probability"
# All other variable names pass through unchanged.

#' @keywords internal
.distribution_plot_xlab <- function(var) {
  suffix_map <- c(
    residual    = "Residual",
    latent      = "Latent Response",
    probability = "Predicted Probability"
  )
  parts <- strsplit(var, ".", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(toupper(var))
  suffix <- parts[length(parts)]
  if (!suffix %in% names(suffix_map)) return(toupper(var))
  base <- paste(parts[-length(parts)], collapse = ".")
  paste(toupper(base), suffix_map[[suffix]])
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
#' Builds five families of residual diagnostic plots for each outcome
#' variable (DV) in an rblimp model:
#'
#' \describe{
#'   \item{Residual vs. predicted}{Scatter plot with a loess smoother.
#'     Targets nonlinearity and heteroscedasticity in the structural
#'     Y-X relationship.}
#'   \item{Residual vs. each predictor}{One plot per predictor for each DV.
#'     Continuous predictors get scatter + loess smoother; declared
#'     discrete predictors (ORDINAL, NOMINAL, COUNT) get stacked points
#'     at integer positions with a mean diamond at each level. Targets
#'     predictor-specific nonlinearity.}
#'   \item{Standardized residual index}{Index plot of standardized residuals
#'     by case number, with horizontal threshold lines at \eqn{\pm 2} and
#'     \eqn{\pm 3}. Labels case row numbers for cases with \eqn{|z| \ge 3}.
#'     Flags cases that are poorly predicted by the model.}
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
#' @param label_family Character. Font family used for outlier labels
#'   and threshold annotations. Default `""` (graphics device default).
#'   Set to a family name (e.g., `"Minion Pro"`) to match a custom
#'   theme applied via `&` in patchwork.
#' @param verbose Logical. If `TRUE`, prints informational messages.
#'   Default `FALSE`.
#'
#' @return A named list of class `"residuals_plot"`. Names follow the
#'   pattern `"<dv>.predicted"`, `"<dv>.<predictor>"`, `"<dv>.index"`,
#'   `"<dv>.leverage"`, and `"<dv>.cooks"`. Calling the list at the
#'   console auto-prints every plot in turn; assigning the result is
#'   silent.
#'
#' @examples
#' \dontrun{
#'   rp <- residuals_plot(my_model)
#'   rp$dpdd.predicted              # residual vs. predicted
#'   rp$dpdd.inflam                 # residual vs. inflam
#'   rp$dpdd.index                  # standardized residual index
#'   rp$dpdd.leverage               # leverage index plot
#'   rp$dpdd.cooks                  # Cook's distance index plot
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
  label_family     = "",
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

  # Average imputations and detect what to plot
  avg_data        <- .residuals_plot_average(model)
  dv_bases        <- .residuals_plot_detect_dvs(avg_data, vars, verbose)
  predictors_list <- .residuals_plot_predictors(model, dv_bases)
  discrete_vars   <- .get_discrete_vars(model)
  nominal_vars    <- .get_discrete_vars(model, types = "nominal")

  # Common style bundle passed to every plot helper
  style <- list(
    font_size    = font_size,
    point_color  = point_color,
    curve_color  = curve_color,
    point_alpha  = point_alpha,
    point_size   = point_size,
    line_width   = line_width,
    label_family = label_family
  )

  plots <- list()
  for (dv in dv_bases) {
    # Residual vs. predicted
    plots[[paste0(dv, ".predicted")]] <-
      .residuals_vs_predicted(avg_data, dv, style)

    # Residual vs. each predictor
    for (pred in predictors_list[[dv]]) {
      if (!pred %in% names(avg_data)) {
        if (verbose) message("Skipping predictor not found in data: ", pred)
        next
      }
      plots[[paste0(dv, ".", pred)]] <-
        .residuals_vs_predictor(avg_data, dv, pred,
                                pred %in% discrete_vars, style)
    }

    # Standardized residual index
    plots[[paste0(dv, ".index")]] <-
      .residuals_index(avg_data, dv, index_thresholds, style)

    # Leverage and Cook's distance
    diag <- .residuals_lm_diagnostics(
      avg_data, dv, predictors_list[[dv]], nominal_vars, verbose
    )
    if (!is.null(diag)) {
      plots[[paste0(dv, ".leverage")]] <- .residuals_leverage(diag, dv, style)
      plots[[paste0(dv, ".cooks")]]    <- .residuals_cooks(diag, dv, style)
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



# Internal: average across imputations =========================================
#
# Returns a single data frame with each numeric column averaged across the
# M imputed datasets. For variables with no missing data the averaged value
# equals the original; for variables with imputed values the averaged value
# represents the post-MI estimate for each case.

#' @keywords internal
.residuals_plot_average <- function(model) {
  imp_list <- model@imputations
  result   <- imp_list[[1]]

  numeric_cols <- vapply(result, is.numeric, logical(1))
  for (col in names(result)[numeric_cols]) {
    vals <- vapply(imp_list, function(d) d[[col]], numeric(nrow(result)))
    result[[col]] <- rowMeans(vals, na.rm = TRUE)
  }
  result
}



# Internal: detect DVs with .residual and .predicted columns ===================

#' @keywords internal
.residuals_plot_detect_dvs <- function(data, vars, verbose) {
  cols       <- names(data)
  resid_cols <- grep("\\.residual$", cols, value = TRUE)
  bases      <- sub("\\.residual$", "", resid_cols)

  # Keep only DVs that also have .predicted
  bases <- bases[paste0(bases, ".predicted") %in% cols]

  if (!is.null(vars)) {
    missing_vars <- setdiff(vars, bases)
    if (length(missing_vars)) {
      stop("Variable(s) not found with .residual and .predicted columns: ",
           paste(missing_vars, collapse = ", "))
    }
    bases <- intersect(bases, vars)
  }

  if (!length(bases)) {
    stop("No DVs found with both .residual and .predicted columns.")
  }
  if (verbose) {
    message("DVs to plot: ", paste(bases, collapse = ", "))
  }
  bases
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

  chunks <- unlist(strsplit(paste(mb, collapse = " "), ";", fixed = TRUE),
                   use.names = FALSE)
  chunks <- gsub("\\s+", " ", trimws(chunks))
  chunks <- chunks[nzchar(chunks)]

  for (ch in chunks) {
    if (!grepl("~", ch, fixed = TRUE)) next
    parts <- strsplit(ch, "~", fixed = TRUE)[[1]]
    if (length(parts) != 2) next

    # DV: last token of LHS (strips block labels like "focal.model:")
    dv_raw    <- trimws(parts[1])
    dv_tokens <- strsplit(dv_raw, "\\s+")[[1]]
    dv        <- sub(":$", "", dv_tokens[length(dv_tokens)])

    if (!dv %in% dv_bases) next

    # Predictors: main RHS only (drop random-slope segment after `|`)
    rhs        <- strsplit(trimws(parts[2]), "\\|", fixed = TRUE)[[1]][1]
    pred_toks  <- strsplit(trimws(rhs), "\\s+")[[1]]
    pred_toks  <- pred_toks[nzchar(pred_toks)]
    pred_toks  <- sub("@.+$", "", pred_toks)            # strip @label
    pred_toks  <- pred_toks[!grepl("\\*", pred_toks)]   # drop interactions
    pred_toks  <- unique(pred_toks)

    result[[dv]] <- pred_toks
  }
  result
}



# Internal: residuals vs. predicted plot =======================================

#' @keywords internal
.residuals_vs_predicted <- function(data, dv, style) {
  df <- data.frame(
    predicted = data[[paste0(dv, ".predicted")]],
    residual  = data[[paste0(dv, ".residual")]]
  )
  df <- df[is.finite(df$predicted) & is.finite(df$residual), ]

  ggplot2::ggplot(df, ggplot2::aes(x = predicted, y = residual)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = "grey50",
      linewidth  = style$line_width * 0.5
    ) +
    ggplot2::geom_point(
      color = style$point_color,
      alpha = style$point_alpha,
      size  = style$point_size
    ) +
    .residuals_smoother(style) +
    ggplot2::labs(
      title = sprintf("Residual vs. Predicted: %s", toupper(dv)),
      x     = "Predicted",
      y     = "Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: residuals vs. predictor plot =======================================

#' @keywords internal
.residuals_vs_predictor <- function(data, dv, predictor, is_discrete, style) {
  df <- data.frame(
    x        = data[[predictor]],
    residual = data[[paste0(dv, ".residual")]]
  )
  df <- df[is.finite(df$x) & is.finite(df$residual), ]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = residual)) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = "grey50",
      linewidth  = style$line_width * 0.5
    )

  if (is_discrete) {
    p <- p +
      ggplot2::geom_point(
        color = style$point_color,
        alpha = style$point_alpha,
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
    p <- p +
      ggplot2::geom_point(
        color = style$point_color,
        alpha = style$point_alpha,
        size  = style$point_size
      ) +
      .residuals_smoother(style)
  }

  p +
    ggplot2::labs(
      title = sprintf("Residual vs. Predictor: %s ~ %s",
                      toupper(dv), toupper(predictor)),
      x     = toupper(predictor),
      y     = "Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: standardized residual index plot ===================================

#' @keywords internal
.residuals_index <- function(data, dv, index_thresholds, style) {
  resid <- data[[paste0(dv, ".residual")]]
  resid <- resid[is.finite(resid)]

  if (length(resid) < 2) {
    stop("Cannot standardize residuals: fewer than 2 finite values for ", dv)
  }

  z  <- (resid - mean(resid)) / stats::sd(resid)
  df <- data.frame(case = seq_along(z), z = z)

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
      alpha = style$point_alpha,
      size  = style$point_size
    )

  # Label cases beyond the largest threshold (two-sided)
  label_threshold <- if (length(index_thresholds)) max(abs(index_thresholds)) else NA
  if (!is.na(label_threshold)) {
    p <- .add_outlier_labels(p, df, "z", label_threshold, style, two_sided = TRUE)
  }

  # Integer y-axis breaks across the data range, unioned with thresholds
  y_breaks <- function(limits) {
    sort(unique(c(seq(floor(limits[1]), ceiling(limits[2])), index_thresholds)))
  }

  p +
    ggplot2::scale_y_continuous(breaks = y_breaks) +
    ggplot2::labs(
      title = sprintf("Standardized Residual Index: %s", toupper(dv)),
      x     = "Record Number",
      y     = "Standardized Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: design matrix builder ==============================================
#
# Builds the design matrix used for leverage and Cook's distance. Continuous,
# ORDINAL, COUNT, and binary NOMINAL predictors are used as-is (raw averaged
# values). NOMINAL predictors with more than 2 distinct integer levels are
# dummy-expanded with the lowest level as the reference category. Averaged
# values for NOMINAL predictors are rounded to integers for the dummy
# comparison so that cases with imputed values still slot into a single
# category.

#' @keywords internal
.build_design_matrix <- function(data, predictors, nominal_vars) {
  cols <- list(intercept = rep(1, nrow(data)))

  for (pred in predictors) {
    if (!pred %in% names(data)) next
    x <- data[[pred]]

    is_nominal_multi <- pred %in% nominal_vars &&
      length(unique(round(x[is.finite(x)]))) > 2

    if (is_nominal_multi) {
      rounded <- round(x)
      lvls    <- sort(unique(rounded[is.finite(rounded)]))
      ref     <- lvls[1]
      for (lev in lvls[-1]) {
        cols[[paste0(pred, ".", lev)]] <- as.numeric(rounded == lev)
      }
    } else {
      cols[[pred]] <- x
    }
  }

  do.call(cbind, cols)
}



# Internal: leverage and Cook's distance =======================================
#
# Computes leverage (diagonal of the hat matrix) and Cook's distance using
# closed-form formulas on the averaged design matrix and Blimp's averaged
# residuals. Returns NULL if the design matrix is singular (which can happen
# with collinear predictors or nominal variables having too few levels).
#
# Leverage h_ii = i-th diagonal of X(X'X)^-1 X'.
# Cook's D_i = (e_i^2 / (p * sigma2)) * (h_ii / (1 - h_ii)^2)
# with sigma2 = sum(e^2) / (n - p) computed from the averaged residuals.

#' @keywords internal
.residuals_lm_diagnostics <- function(data, dv, predictors, nominal_vars, verbose) {
  X <- .build_design_matrix(data, predictors, nominal_vars)
  e <- data[[paste0(dv, ".residual")]]

  keep <- stats::complete.cases(X) & is.finite(e)
  X    <- X[keep, , drop = FALSE]
  e    <- e[keep]
  n    <- length(e)
  p    <- ncol(X)

  if (n <= p) {
    if (verbose) {
      message("Skipping leverage/Cook's D for `", dv,
              "`: not enough complete cases relative to parameters.")
    }
    return(NULL)
  }

  XtX_inv <- tryCatch(solve(crossprod(X)), error = function(err) NULL)
  if (is.null(XtX_inv)) {
    if (verbose) {
      message("Skipping leverage/Cook's D for `", dv,
              "`: design matrix is singular.")
    }
    return(NULL)
  }

  h      <- rowSums((X %*% XtX_inv) * X)
  sigma2 <- sum(e^2) / (n - p)
  D      <- (e^2 / (p * sigma2)) * (h / (1 - h)^2)

  # Restore full-length vectors with NA for filtered-out rows
  full_h <- rep(NA_real_, length(data[[paste0(dv, ".residual")]]))
  full_D <- full_h
  full_h[keep] <- h
  full_D[keep] <- D

  list(leverage = full_h, cooks = full_D, p = p, n = n)
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
      x      = 0,
      y      = cuts,
      label  = c("2p/n", "3p/n"),
      hjust  = 1,
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
      title = sprintf("Leverage: %s", toupper(dv)),
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
      x      = 0,
      y      = cut,
      label  = "4/n",
      hjust  = 1,
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
      title = sprintf("Cook's Distance: %s", toupper(dv)),
      x     = "Record Number",
      y     = "Cook's D"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: outlier labels =====================================================
#
# Adds row-number labels to cases exceeding a threshold on a diagnostic plot.
# Uses ggrepel::geom_text_repel if the package is installed (cleaner label
# placement); falls back to ggplot2::geom_text otherwise. The fallback can
# overlap when many cases exceed the threshold but adds no dependency.
#
# Used by .residuals_index, .residuals_leverage, and .residuals_cooks.

#' @keywords internal
.add_outlier_labels <- function(p, df, value_col, threshold, style, two_sided = FALSE) {
  values <- df[[value_col]]

  outliers <- if (two_sided) {
    df[abs(values) >= abs(threshold), ]
  } else {
    df[values >= threshold, ]
  }

  if (!nrow(outliers)) return(p)

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



# Internal: smoother layer =====================================================
#
# Loess smoother with consistent visual styling (curve color, linewidth,
# no confidence band). Used by both the residual-vs-predicted and the
# continuous residual-vs-predictor plots.

#' @keywords internal
.residuals_smoother <- function(style) {
  ggplot2::geom_smooth(
    method    = "loess",
    formula   = y ~ x,
    color     = style$curve_color,
    linewidth = style$line_width,
    se        = FALSE
  )
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
