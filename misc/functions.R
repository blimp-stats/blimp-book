# rblimp plotting functions ====================================================
#
# Cleaned versions of plotting functions from the original `functions.R`,
# prepared for production handoff to the rblimp package programmer.
# Cleanup decisions are documented in `rblimp_cleanup_changelog.md` in this
# same folder.
#
# Functions cleaned so far:
#   - distribution_plot   (supersedes imputation_plot and univariate_plot)
#   - residuals_plot      (averaged imputations; branches on outcome type:
#                          continuous/ORDINAL get linear-style diagnostics,
#                          NOMINAL get probability-scale binned residuals
#                          per Gelman & Hill 2007, COUNT outcomes skipped)
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
  
  if (is.null(vars)) {
    model_vars       <- .extract_model_vars(model)
    model_implied_rx <- "\\.(residual|latent|probability|yjt)$"
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
#   <var>.residual    -> "<var> Raw Residual"
#   <var>.latent      -> "<var> Latent Response"
#   <var>.probability -> "<var> Predicted Probability"
#   <var>.yjt         -> "<var> Yeo-Johnson Transformed"
# All other variable names pass through unchanged. The "Raw Residual" wording
# matches the y-axis convention in `residuals_plot`, where the unstandardized
# residual-vs-predicted panel reads "Raw Residual" and the standardized
# panels read "Standardized Residual".

#' @keywords internal
.distribution_plot_xlab <- function(var) {
  suffix_map <- c(
    residual    = "Raw Residual",
    latent      = "Latent Response",
    probability = "Predicted Probability",
    yjt         = "Yeo-Johnson Transformed"
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
#'     under a correctly specified variance model a previously fanned
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
#'   outcomes contribute no entries. Calling the list at the console
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
  ordinal_vars    <- .get_discrete_vars(model, types = "ordinal")
  count_vars      <- .get_discrete_vars(model, types = "count")
  
  # Common style bundle passed to every plot helper
  style <- list(
    font_size    = font_size,
    point_color  = point_color,
    curve_color  = curve_color,
    point_alpha  = point_alpha,
    point_size   = point_size,
    line_width   = line_width,
    max_labels   = max_labels,
    label_family = label_family
  )
  
  plots <- list()
  overlay_done <- character(0)  # parents that have had their probability overlays generated
  for (dv in dv_bases) {
    outcome <- .classify_outcome_type(dv, ordinal_vars, nominal_vars, count_vars)
    
    # NOMINAL and ORDINAL outcomes: build one Predicted Probability vs.
    # Predictor overlay per predictor (per parent, deduplicated so multi-
    # category NOMINAL DVs do not generate the same overlay K - 1 times).
    if (outcome$type %in% c("nominal", "ordinal") &&
        !outcome$parent %in% overlay_done) {
      for (pred in predictors_list[[dv]]) {
        if (!pred %in% names(avg_data)) {
          if (verbose) message("Skipping predictor not found in data: ", pred)
          next
        }
        pp <- .predicted_probability_vs_predictor(
          avg_data, outcome$parent, pred, pred %in% discrete_vars, style
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
        .pearson_residuals_index(avg_data, outcome$parent, alpha,
                                 index_thresholds, style)
      
      # Pearson residual vs. each predictor
      for (pred in predictors_list[[dv]]) {
        if (!pred %in% names(avg_data)) {
          if (verbose) message("Skipping predictor not found in data: ", pred)
          next
        }
        plots[[paste0(outcome$parent, ".", pred)]] <-
          .pearson_residuals_vs_predictor(
            avg_data, outcome$parent, pred, alpha,
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
      bp <- .binned_residuals(avg_data, outcome$parent, outcome$category, style)
      if (!is.null(bp)) {
        plots[[paste0(dv, ".binned")]] <- bp
      } else if (verbose) {
        message("Skipping binned residuals for `", dv,
                "`: too few cases or missing probability column.")
      }
      
      # Binned residuals vs. each predictor (one plot per predictor)
      for (pred in predictors_list[[dv]]) {
        if (!pred %in% names(avg_data)) {
          if (verbose) message("Skipping predictor not found in data: ", pred)
          next
        }
        bvp <- .binned_residuals_vs_predictor(
          avg_data, outcome$parent, outcome$category, pred,
          pred %in% discrete_vars, style
        )
        if (!is.null(bvp)) {
          plots[[paste0(dv, ".", pred)]] <- bvp
        }
      }
      next
    }
    
    # Continuous and ORDINAL outcomes: linear-style diagnostic suite.
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
                                pred %in% discrete_vars,
                                index_thresholds, style)
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
  chunks <- chunks[nzchar(chunks)]
  
  for (ch in chunks) {
    if (!grepl("~", ch, fixed = TRUE)) next
    parts <- strsplit(ch, "~", fixed = TRUE)[[1]]
    if (length(parts) != 2) next
    
    # DV: last token of LHS (strips block labels like "focal.model:")
    dv_raw    <- trimws(parts[1])
    dv_tokens <- strsplit(dv_raw, "\\s+")[[1]]
    dv        <- sub(":$", "", dv_tokens[length(dv_tokens)])
    
    # Strip yjt(...) wrapper. A model line "yjt(dpddcent) ~ inflam ..." fits
    # the regression on the Yeo-Johnson-transformed scale but still produces
    # dpddcent.residual / dpddcent.predicted columns. The DV detected from
    # those columns is "dpddcent", so we unwrap yjt() here to make the match.
    dv <- sub("^yjt\\((.*)\\)$", "\\1", dv)
    
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
      y     = "Raw Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: standardized residual vs. predictor plot ===========================
#
# Plots standardized residuals (residual / sigma_i; see `.casewise_sigma`)
# against a predictor. Always-standardized so the y-axis is on the same
# (SD-unit) scale as the standardized residual index plot, with matching
# +/- index_thresholds reference bands. Under a homoscedastic fit the
# standardization is by a single scalar sd, so the cloud shape is identical
# to a raw-residual plot; under a heteroscedastic fit each residual is
# divided by its own model-implied sigma_i, so a successfully modeled funnel
# should collapse to a flat band.

#' @keywords internal
.residuals_vs_predictor <- function(data, dv, predictor, is_discrete,
                                    index_thresholds, style) {
  resid   <- data[[paste0(dv, ".residual")]]
  sigma_i <- .casewise_sigma(data, dv)
  
  df <- data.frame(
    x        = data[[predictor]],
    residual = resid / sigma_i
  )
  df <- df[is.finite(df$x) & is.finite(df$residual), ]
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = residual)) +
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
      title = sprintf("Standardized Residual vs. Predictor: %s ~ %s",
                      toupper(dv), toupper(predictor)),
      x     = toupper(predictor),
      y     = "Standardized Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: probability-scale binned residuals plot ============================
#
# Implements Gelman and Hill's (2007) binned-residuals diagnostic for a
# NOMINAL outcome category:
#
#   1. Compute raw residuals on the probability scale,
#         r_i = I(Y_i = k) - p_hat_i,
#      where p_hat_i is the predicted P(Y_i = k) (read from the
#      "<parent>.<k>.probability" column) and I() is the category
#      indicator on the observed parent variable.
#   2. Sort cases by predicted probability and divide into
#      n_bins = floor(sqrt(n)) equal-sized bins.
#   3. For each bin, compute the mean predicted probability, the mean
#      raw residual, and a +-2 * SD/sqrt(n_bin) envelope (G&H default).
#   4. Plot bin means against bin midpoints with the envelope and a
#      reference line at y = 0.
#
# The parent column comes from `model@average_imp`, which holds modal
# imputation for multicategoricals and the observed code for binary
# variables. The category-indicator comparison is therefore exact.
#
# Returns NULL when there are too few cases (< 16) for a meaningful binned
# plot, or when the parent / probability columns are missing.

#' @keywords internal
.binned_residuals <- function(data, parent, category, style) {
  pred_col <- paste0(parent, ".", category, ".probability")
  if (!pred_col %in% names(data))         return(NULL)
  if (!parent   %in% names(data))         return(NULL)
  
  pred       <- data[[pred_col]]
  observed_k <- as.numeric(data[[parent]] == category)
  
  keep       <- is.finite(pred) & is.finite(observed_k)
  pred       <- pred[keep]
  observed_k <- observed_k[keep]
  
  n <- length(pred)
  if (n < 16) return(NULL)
  
  n_bins <- floor(sqrt(n))
  
  # Sort cases by predicted probability and assign equal-sized bins
  ord         <- order(pred)
  bin_id      <- integer(n)
  bin_id[ord] <- ceiling((seq_len(n) / n) * n_bins)
  
  raw_resid <- observed_k - pred
  
  # Per-bin summaries: one bin mean per bin, plotted at the bin's mean
  # predicted probability. No error bands -- consistent with the
  # linear-regression diagnostics in residuals_vs_predicted, which
  # also operate on single averaged imputations and therefore do not
  # report sampling-uncertainty overlays.
  bin_df <- data.frame(bin = bin_id, pred = pred, resid = raw_resid)
  bin_summary <- do.call(rbind, lapply(split(bin_df, bin_df$bin), function(d) {
    data.frame(
      pred_mean  = mean(d$pred),
      resid_mean = mean(d$resid),
      n_bin      = nrow(d)
    )
  }))
  
  # Title: include category index when parent is multi-category (or always,
  # for consistency with how nominal columns are named)
  title_dv <- if (is.na(category)) toupper(parent) else
    sprintf("%s.%d", toupper(parent), as.integer(category))
  
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
    .residuals_smoother(style) +
    ggplot2::labs(
      title = sprintf("Binned Residual vs. Predicted: %s", title_dv),
      x     = "Mean Predicted Probability",
      y     = "Mean Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: binned residuals vs. each predictor ================================
#
# Probability-scale analogue of .residuals_vs_predictor for NOMINAL outcomes.
# For each predictor, residuals r_i = I(Y_i = k) - p_hat_i are binned by
# the predictor value rather than by the predicted probability, and bin
# means are plotted against bin midpoints with a +-2 * SD/sqrt(n_bin)
# envelope.
#
# Continuous predictors are sorted and divided into floor(sqrt(n))
# equal-sized bins; the envelope is drawn as connected upper/lower lines
# around y = 0, matching the calibration plot style. Discrete predictors
# (declared ORDINAL, NOMINAL, or COUNT) get one bin per observed level
# and use error bars around each level's mean residual instead of the
# zero-centered envelope; the visual question is the same ("is this
# level's mean residual within sampling noise of zero?") but the
# convention for categorical x-axes is the cleaner display.
#
# Systematic curvature against a continuous predictor (or a sloped pattern
# across levels of a discrete one) indicates that the predictor is being
# modeled with the wrong functional form -- e.g., a quadratic term should
# be added, or the variable should enter on a transformed scale.
#
# Returns NULL when there are too few cases (< 16) for a meaningful binned
# plot, or when any required column is missing.

#' @keywords internal
.binned_residuals_vs_predictor <- function(data, parent, category, predictor,
                                           is_discrete, style) {
  pred_col <- paste0(parent, ".", category, ".probability")
  if (!pred_col %in% names(data))         return(NULL)
  if (!parent   %in% names(data))         return(NULL)
  if (!predictor %in% names(data))        return(NULL)
  
  pred       <- data[[pred_col]]
  observed_k <- as.numeric(data[[parent]] == category)
  x_vals     <- data[[predictor]]
  
  keep       <- is.finite(pred) & is.finite(observed_k) & is.finite(x_vals)
  pred       <- pred[keep]
  observed_k <- observed_k[keep]
  x_vals     <- x_vals[keep]
  
  n <- length(x_vals)
  if (n < 16) return(NULL)
  
  raw_resid <- observed_k - pred
  title_dv  <- sprintf("%s.%d", toupper(parent), as.integer(category))
  
  if (is_discrete) {
    # One mean residual per observed level (no error bands; the
    # single-averaged-imputations source does not support inferential
    # overlays, matching the linear-regression diagnostics in Section 2.7).
    levels_vec <- sort(unique(x_vals))
    if (length(levels_vec) < 2) return(NULL)
    
    rows <- lapply(levels_vec, function(lev) {
      cases <- x_vals == lev
      if (sum(cases) < 2) return(NULL)
      data.frame(
        x_mean     = lev,
        resid_mean = mean(raw_resid[cases]),
        n_bin      = sum(cases)
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) < 2) return(NULL)
    bin_summary <- do.call(rbind, rows)
    
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
    # Continuous predictor: bin by predictor value into floor(sqrt(n)) bins.
    # Same no-error-band rule as the discrete branch above.
    n_bins      <- floor(sqrt(n))
    ord         <- order(x_vals)
    bin_id      <- integer(n)
    bin_id[ord] <- ceiling((seq_len(n) / n) * n_bins)
    
    bin_df <- data.frame(bin = bin_id, x = x_vals, resid = raw_resid)
    bin_summary <- do.call(rbind, lapply(split(bin_df, bin_df$bin), function(d) {
      data.frame(
        x_mean     = mean(d$x),
        resid_mean = mean(d$resid),
        n_bin      = nrow(d)
      )
    }))
    
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
      .residuals_smoother(style)
  }
  
  p +
    ggplot2::labs(
      title = sprintf("Binned Residual vs. Predictor: %s ~ %s", title_dv, toupper(predictor)),
      x     = toupper(predictor),
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
# Continuous predictors get case-level scatter plus a per-category loess
# smoother. Discrete predictors get one mean point per (category, level)
# combination, connected by per-category lines.
#
# The function discovers categories from the `<parent>.<k>.probability`
# columns present in `data`; NOMINAL outcomes contribute K - 1 probability
# columns and ORDINAL outcomes contribute K, and the plot is built from
# whatever is available.
#
# Returns NULL if no probability columns are found or the predictor is
# missing from `data`.

#' @keywords internal
.predicted_probability_vs_predictor <- function(data, parent, predictor,
                                                is_discrete, style) {
  if (!predictor %in% names(data)) return(NULL)
  
  # Detect <parent>.<k>.probability columns
  prob_rx   <- paste0("^", parent, "\\.([0-9]+)\\.probability$")
  prob_cols <- grep(prob_rx, names(data), value = TRUE)
  if (!length(prob_cols)) return(NULL)
  
  # Extract category indices and sort
  k_levels  <- as.integer(sub(prob_rx, "\\1", prob_cols))
  k_order   <- order(k_levels)
  prob_cols <- prob_cols[k_order]
  k_levels  <- k_levels[k_order]
  K         <- length(prob_cols)
  
  # Build long-format data: one row per case per category
  x_vals  <- data[[predictor]]
  long_df <- do.call(rbind, lapply(seq_len(K), function(i) {
    df <- data.frame(
      x        = x_vals,
      prob     = data[[prob_cols[i]]],
      category = factor(as.character(k_levels[i]),
                        levels = as.character(k_levels))
    )
    df[is.finite(df$x) & is.finite(df$prob), ]
  }))
  if (!nrow(long_df)) return(NULL)
  
  # Visual encodings: ggplot default hues + distinct shapes and line types.
  # Color comes from ggplot's default hue palette (consistent with the rest
  # of the function returning color plots; convert externally for grayscale
  # printing). Shape and line type provide redundant differentiation so the
  # legend remains readable after grayscale conversion. Extended to seven
  # values; collapses cleanly if K < 7.
  shape_values    <- c(16, 17, 15, 18, 4, 8, 3)[seq_len(K)]
  linetype_values <- c("solid", "dashed", "dotted", "dotdash",
                       "longdash", "twodash", "1F")[seq_len(K)]
  
  title_dv <- toupper(parent)
  
  if (is_discrete) {
    # Discrete predictor: case-level scatter at each integer x position
    # plus per-(category, level) means connected by lines.
    cell_means <- stats::aggregate(
      prob ~ x + category, data = long_df, FUN = mean
    )
    p <- ggplot2::ggplot(
      long_df,
      ggplot2::aes(x = x, y = prob,
                   color = category, shape = category, linetype = category)
    ) +
      ggplot2::geom_point(
        alpha = style$point_alpha * 0.35,
        size  = style$point_size
      ) +
      ggplot2::geom_line(
        data      = cell_means,
        linewidth = style$line_width
      ) +
      ggplot2::geom_point(
        data = cell_means,
        size = style$point_size * 2.5
      ) +
      ggplot2::scale_x_continuous(breaks = .integer_breaks)
  } else {
    # Continuous predictor: case-level scatter + per-category loess
    p <- ggplot2::ggplot(
      long_df,
      ggplot2::aes(x = x, y = prob,
                   color = category, shape = category, linetype = category)
    ) +
      ggplot2::geom_point(
        alpha = style$point_alpha * 0.35,
        size  = style$point_size
      ) +
      ggplot2::geom_smooth(
        method    = "loess",
        formula   = y ~ x,
        se        = FALSE,
        linewidth = style$line_width
      )
  }
  
  p +
    ggplot2::scale_shape_manual(values = shape_values) +
    ggplot2::scale_linetype_manual(values = linetype_values) +
    ggplot2::labs(
      title    = sprintf("Predicted Probability vs. Predictor: %s ~ %s",
                         title_dv, toupper(predictor)),
      x        = toupper(predictor),
      y        = "Predicted Probability",
      color    = paste(title_dv, "Category"),
      shape    = paste(title_dv, "Category"),
      linetype = paste(title_dv, "Category")
    ) +
    .residuals_plot_theme(style$font_size) +
    ggplot2::theme(
      legend.position = "top",
      legend.text     = ggplot2::element_text(size = style$font_size - 2),
      legend.title    = ggplot2::element_text(size = style$font_size - 2)
    )
}



# Internal: per-observation residual SD ========================================
#
# Returns the residual standardization scale for a DV. When the model includes
# a variance specification (`logvar(<dv>) ~ ...`), Blimp writes the log
# variance prediction to `var.<dv>.predicted`; in that case each observation
# gets its own model-implied sigma_i = sqrt(exp(log variance_i)). Otherwise
# the function falls back to a scalar sd of the residuals, recycled to length
# n, which preserves the homoscedastic standardization used previously. This
# helper backs both the standardized residual index plot and the residual
# vs. predictor plot so the two diagnostics are on the same scale.

#' @keywords internal
.casewise_sigma <- function(data, dv) {
  resid_col <- paste0(dv, ".residual")
  var_col   <- paste0("var.", dv, ".predicted")
  n         <- length(data[[resid_col]])
  if (var_col %in% names(data)) {
    sqrt(exp(data[[var_col]]))
  } else {
    rep(stats::sd(data[[resid_col]], na.rm = TRUE), n)
  }
}



# Internal: standardized residual index plot ===================================
#
# Standardizes each residual by its model-implied sigma_i (per-observation when
# a variance model is fit, scalar marginal sd otherwise; see `.casewise_sigma`).
# Reference lines at +/- index_thresholds; cases with |z| beyond the largest
# absolute threshold get row-number labels.

#' @keywords internal
.residuals_index <- function(data, dv, index_thresholds, style) {
  resid   <- data[[paste0(dv, ".residual")]]
  sigma_i <- .casewise_sigma(data, dv)
  
  keep    <- is.finite(resid) & is.finite(sigma_i) & sigma_i > 0
  resid   <- resid[keep]
  sigma_i <- sigma_i[keep]
  
  if (length(resid) < 2) {
    stop("Cannot standardize residuals: fewer than 2 finite values for ", dv)
  }
  
  z  <- resid / sigma_i
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



# Internal: Pearson residual index plot (COUNT outcomes) =======================
#
# Mirrors .residuals_index, but the y-axis is the Pearson residual computed
# from Blimp's predicted mean count and the model's NB variance-dispersion
# parameter alpha (stored in @estimates as "<dv> Variance Dispersion
# Parameter"). Under the (mu, alpha) NB parameterization, Var(Y) = mu +
# alpha * mu^2, and the Pearson residual is
#
#   z_i = (y_i - mu_hat_i) / sqrt(mu_hat_i + alpha_hat * mu_hat_i^2).
#
# Recommended diagnostic in Gelman and Hill (2006, Section 6.2, Figure 6.1b).
# Reference lines at +/- index_thresholds; cases with |z| beyond the largest
# absolute threshold get row-number labels.

#' @keywords internal
.pearson_residuals_index <- function(data, dv, alpha, index_thresholds, style) {
  y     <- data[[dv]]
  mu    <- data[[paste0(dv, ".predicted")]]
  z     <- (y - mu) / sqrt(mu + alpha * mu^2)
  z     <- z[is.finite(z)]
  df    <- data.frame(case = seq_along(z), z = z)
  
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
      title = sprintf("Standardized Pearson Residual Index: %s", toupper(dv)),
      x     = "Record Number",
      y     = "Standardized Pearson Residual"
    ) +
    .residuals_plot_theme(style$font_size)
}



# Internal: Pearson residual vs. predictor (COUNT outcomes) ====================
#
# Mirrors .residuals_vs_predictor, but plots Pearson residuals (formula above)
# against each predictor. Continuous predictors get scatter plus a loess
# smoother; discrete predictors get stacked points with a curve-colored mean
# diamond at each integer level. Recommended diagnostic in Gelman and Hill
# (2006, Section 6.2).

#' @keywords internal
.pearson_residuals_vs_predictor <- function(data, dv, predictor, alpha,
                                            is_discrete, style) {
  y  <- data[[dv]]
  mu <- data[[paste0(dv, ".predicted")]]
  z  <- (y - mu) / sqrt(mu + alpha * mu^2)
  df <- data.frame(
    x = data[[predictor]],
    z = z
  )
  df <- df[is.finite(df$x) & is.finite(df$z), ]
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = z)) +
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
      title = sprintf("Standardized Pearson Residual vs. Predictor: %s ~ %s",
                      toupper(dv), toupper(predictor)),
      x     = toupper(predictor),
      y     = "Standardized Pearson Residual"
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
