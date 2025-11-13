# GLOBAL STYLING AND HELPERS ----

plot_colors <- c(
  red    = "#F8766D",
  green  = "#7CAE00",
  teal   = "#00BFC4",
  violet = "#C77CFF",
  orange = "#E58700",
  blue   = "#619CFF",
  yellow = "#FFD700",
  pink   = "#FF61C3"
)

# alpha used for filled layers (bars, ribbons, etc.) across every plot
plot_shading <- 0.25

# SHARED THEME ----
# SHARED THEME ----

blimp_theme <- function(font_size = 14) {
  base_theme <- ggplot2::theme_minimal(base_size = font_size)
  replace_theme <- ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks.y     = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_text(
      size   = font_size,
      margin = ggplot2::margin(t = 2)
    ),
    axis.title.x     = ggplot2::element_text(
      size   = font_size,
      margin = ggplot2::margin(t = 12)
    ),
    plot.title       = ggplot2::element_text(
      hjust = 0.5,
      face  = "bold",
      size  = font_size + 2
    ),
    legend.title     = ggplot2::element_text(size = font_size),
    legend.text      = ggplot2::element_text(size = font_size),
    legend.key.height = ggplot2::unit(1.1, "lines"),
    legend.key.width  = ggplot2::unit(1.6, "lines"),
    legend.position   = c(0.95, 0.85),
    legend.justification = c("right", "top"),
    legend.background = ggplot2::element_blank(),
    legend.key        = ggplot2::element_blank()
  )
  
  ggplot2::`%+replace%`(base_theme, replace_theme)
}

# EXTRACT VARIABLES FROM MODEL SECTION ----

# helper: coerce @syntax to a single text blob
.syntax_as_text <- function(sx) {
  if (inherits(sx, "blimp_syntax") || is.list(sx)) {
    paste(unlist(sx, use.names = FALSE), collapse = "\n")
  } else if (is.character(sx)) {
    paste(sx, collapse = "\n")
  } else {
    ""
  }
}

# helper: pull the MODEL block lines (handles focal:/predictors: etc.)
.get_model_block <- function(model) {
  if (is.null(model@syntax)) return(character(0))
  sx <- model@syntax
  
  # Case 1: modern blimp_syntax/list with a $model component
  if ((inherits(sx, "blimp_syntax") || is.list(sx)) && "model" %in% names(sx)) {
    mb <- as.character(sx$model)
    mb <- mb[!is.na(mb)]
    mb <- trimws(mb)
    mb <- mb[nzchar(mb)]
    return(mb)
  }
  
  # Case 2: plain text / legacy syntax with "MODEL:" header
  txt <- .syntax_as_text(sx)
  if (!nzchar(txt)) return(character(0))
  
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  if (!length(lines)) return(character(0))
  
  idx_model <- grep("^\\s*MODEL\\s*:", lines, ignore.case = TRUE)
  if (!length(idx_model)) return(character(0))
  
  lines2 <- lines[idx_model[1]:length(lines)]
  # remove the literal "MODEL:" label from the first line
  lines2[1] <- sub("^\\s*MODEL\\s*:\\s*", "", lines2[1], ignore.case = TRUE)
  
  # stop at next ALL-CAPS header (SEED:, OPTIONS:, SAVE:, etc.)
  if (length(lines2) > 1L) {
    hdr_idx <- which(grepl("^\\s*[A-Z]+\\s*:", lines2[-1L]))
    if (length(hdr_idx)) {
      stop_at <- hdr_idx[1L]
      lines2  <- lines2[seq_len(stop_at + 1L)]
    }
  }
  
  lines2
}

.extract_model_vars <- function(model) {
  if (is.null(model@syntax))
    stop("@syntax not found on model object")
  
  mb <- .get_model_block(model)
  if (!length(mb))
    stop("Could not locate MODEL section in @syntax.")
  
  model_text <- paste(mb, collapse = " ")
  
  # allow letters / digits / underscore / dot
  clean <- gsub("[^A-Za-z0-9_.]+", " ", model_text)
  toks  <- unlist(strsplit(clean, "\\s+"), use.names = FALSE)
  toks  <- toks[nzchar(toks)]
  
  # keep only things that look like variable names
  toks  <- toks[grepl("[A-Za-z_]", toks)]
  
  # drop obvious non-variable keywords
  reserved <- c(
    "MODEL", "model",
    "focal", "Focal",
    "predictors", "Predictors",
    "WITHIN", "within",
    "BETWEEN", "between",
    "random", "fixed"
  )
  toks <- setdiff(toks, reserved)
  
  unique(toks)
}

# ORDINAL / NOMINAL HELPERS ----

.get_categorical_vars <- function(model) {
  if (is.null(model@syntax)) return(character(0))
  sx <- model@syntax
  
  # Case 1: blimp_syntax/list with ordinal/nominal components
  if (inherits(sx, "blimp_syntax") || is.list(sx)) {
    keys <- intersect(names(sx), c("ORDINAL", "ordinal", "NOMINAL", "nominal"))
    if (length(keys)) {
      vals <- unlist(sx[keys], use.names = FALSE)
      vals <- vals[!is.na(vals)]
      if (length(vals)) {
        toks <- unlist(
          strsplit(paste(vals, collapse = " "), "[ ,;]+", perl = TRUE),
          use.names = FALSE
        )
        toks <- toks[nzchar(toks)]
        return(unique(toks))
      }
    }
  }
  
  # Case 2: plain text – look for ORDINAL: / NOMINAL: blocks
  txt <- .syntax_as_text(sx)
  if (!nzchar(txt)) return(character(0))
  txt1 <- gsub("\\s+", " ", txt)
  
  grab <- function(key) {
    rx <- paste0("(?i)\\b", key, "\\s*:\\s*([^;]+)\\s*;")
    m  <- gregexpr(rx, txt1, perl = TRUE)
    segs <- regmatches(txt1, m)[[1]]
    if (!length(segs)) return(character(0))
    out <- unlist(lapply(segs, function(seg) {
      s <- sub(rx, "\\1", seg, perl = TRUE)
      s <- gsub("[,]", " ", s)
      toks <- strsplit(s, "\\s+", perl = TRUE)[[1]]
      toks[nzchar(toks)]
    }), use.names = FALSE)
    unique(out)
  }
  
  unique(c(grab("ORDINAL"), grab("NOMINAL")))
}

# STANDARDIZE RESIDUALS (MI) ----
# Pools residual mean/variance across imputations and returns stacked z-scores.

standardize_residuals <- function(model, vars = NULL, na.rm = TRUE) {
  if (!is.list(model@imputations) || length(model@imputations) == 0)
    stop("@imputations must be a non-empty list of data frames")
  cols1 <- names(model@imputations[[1]])
  
  resid_cols <- grep("\\.residual$", cols1, value = TRUE)
  
  if (is.null(vars)) {
    bases <- sub("\\.residual$", "", resid_cols)
  } else {
    stopifnot(is.character(vars), length(vars) >= 1)
    bases <- unique(sub("\\.residual$", "", vars))
    have <- paste0(bases, ".residual")
    bases <- bases[have %in% resid_cols]
    if (!length(bases))
      stop("None of the requested variables have a corresponding '*.residual' column in imputations.")
  }
  
  get_resid <- function(imp_df, base) {
    nm <- paste0(base, ".residual")
    if (!nm %in% names(imp_df)) return(NULL)
    imp_df[[nm]]
  }
  
  out_rows <- list()
  stats    <- data.frame(
    base        = bases,
    pooled_mean = NA_real_,
    pooled_sd   = NA_real_,
    m           = NA_integer_,
    n_total     = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  row_counter <- 1L
  for (b in bases) {
    r_list <- lapply(model@imputations, get_resid, base = b)
    keep   <- vapply(r_list, function(x) !is.null(x), logical(1L))
    r_list <- r_list[keep]
    m      <- length(r_list)
    if (m == 0L) next
    
    mu_j <- vapply(r_list, function(x) mean(x, na.rm = na.rm),  numeric(1L))
    v_j  <- vapply(r_list, function(x) stats::var(x,  na.rm = na.rm), numeric(1L))
    
    pooled_mean <- mean(mu_j, na.rm = TRUE)
    pooled_var  <- mean(v_j,  na.rm = TRUE)
    pooled_sd   <- sqrt(pooled_var)
    
    for (j in seq_len(m)) {
      r <- r_list[[j]]
      n <- length(r)
      z <- if (is.finite(pooled_sd) && pooled_sd > 0) (r - pooled_mean) / pooled_sd else rep(NA_real_, n)
      
      out_rows[[row_counter]] <- data.frame(
        base   = b,
        imp    = j,
        row    = seq_len(n),
        resid  = r,
        z      = z,
        stringsAsFactors = FALSE
      )
      row_counter <- row_counter + 1L
    }
    
    stats[stats$base == b, c("pooled_mean","pooled_sd","m","n_total")] <-
      list(pooled_mean, pooled_sd, m, sum(vapply(r_list, length, integer(1L))))
    if (!is.finite(pooled_sd) || pooled_sd <= 0)
      warning("Pooled SD is non-positive for '", b, "'. z-scores set to NA.")
  }
  
  data_stacked <- if (length(out_rows)) do.call(rbind, out_rows) else
    data.frame(base=character(), imp=integer(), row=integer(),
               resid=numeric(), z=numeric(), stringsAsFactors = FALSE)
  
  structure(list(data = data_stacked, stats = stats), class = "blimp_stdres")
}

# IMPUTATION HISTOGRAM(S) ----

imputation_plot <- function(model, var = NULL, bins = 50, main = NULL,
                            fill_color = "teal", font_size = 14) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!is.list(model@imputations) || !length(model@imputations)) stop("@imputations must be non-empty")
  if (!fill_color %in% names(plot_colors)) stop("fill_color must be one of: ", paste(names(plot_colors), collapse = ", "))
  
  vars_all <- names(model@imputations[[1]])
  
  if (is.null(var)) {
    model_vars <- unique(.extract_model_vars(model))
    # base name = everything before first dot
    base_all  <- sub("\\..*$", "", vars_all)
    var <- vars_all[base_all %in% model_vars]
    if (!length(var)) stop("No matching variables found based on MODEL section.")
    message("Plotting variables based on MODEL section: ", paste(var, collapse = ", "))
  } else if (!all(var %in% vars_all)) {
    bad <- setdiff(var, vars_all)
    stop("Variable(s) not found in imputations: ", paste(bad, collapse = ", "))
  }
  
  fill_col <- unname(plot_colors[fill_color])
  n_sets   <- length(model@imputations)
  
  build_one <- function(v) {
    imp_vals <- unlist(lapply(model@imputations, `[[`, v), use.names = FALSE)
    if (!is.numeric(imp_vals)) { message("Skipping non-numeric variable: ", v); return(NULL) }
    
    main_text <- if (is.null(main))
      paste0("Distribution Over ", n_sets, " Imputed Data Sets: ", v)
    else main
    
    ggplot2::ggplot(data.frame(x = imp_vals),
                    ggplot2::aes(x = x, y = ggplot2::after_stat(density))) +
      ggplot2::geom_histogram(
        bins = bins,
        fill = fill_col,
        color = fill_col,
        alpha = plot_shading
      ) +
      ggplot2::labs(title = main_text, x = v, y = NULL) +
      blimp_theme(font_size)
  }
  
  plots <- setNames(lapply(var, build_one), var)
  for (p in plots) if (!is.null(p)) print(p)
  invisible(plots)
}

# OBSERVED VS. IMPUTED ----

imputed_vs_observed_plot <- function(model, var = NULL, bins = 50, main = NULL,
                                     observed_fill = "teal", imputed_line = "violet",
                                     font_size = 14) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!is.list(model@imputations) || !length(model@imputations))
    stop("@imputations must be non-empty")
  if (is.null(model@variance_imp))
    stop("@variance_imp not found on model object")
  if (!observed_fill %in% names(plot_colors) || !imputed_line %in% names(plot_colors))
    stop("observed_fill and imputed_line must be names in plot_colors: ",
         paste(names(plot_colors), collapse = ", "))
  
  # ---- figure out which variables are eligible ----
  vars_imp1   <- names(model@imputations[[1]])
  vars_varimp <- names(model@variance_imp)
  vars_all    <- intersect(vars_varimp, vars_imp1)
  
  # drop derived columns; we only want the "base" variables
  derived_pattern <- "\\.(latent|residual|predicted|probability)$"
  vars_base <- vars_all[!grepl(derived_pattern, vars_all)]
  
  if (is.null(var)) {
    # use robust MODEL parser
    model_vars <- .extract_model_vars(model)
    # exact name matching to avoid age/cigage problems
    var <- intersect(vars_base, model_vars)
    if (!length(var))
      stop("No matching variables found that are both in the MODEL section and in @variance_imp/@imputations.")
    message("Plotting variables with observed values from MODEL section: ",
            paste(var, collapse = ", "))
  } else {
    # user-specified vars: check they exist in base set
    missing_vars <- setdiff(var, vars_base)
    if (length(missing_vars)) {
      stop("Variable(s) not found as base variables in @variance_imp/@imputations: ",
           paste(missing_vars, collapse = ", "))
    }
  }
  
  obs_col <- unname(plot_colors[observed_fill])
  imp_col <- unname(plot_colors[imputed_line])
  n_imps  <- length(model@imputations)
  
  build_one <- function(v) {
    vimp <- model@variance_imp[[v]]
    if (is.null(vimp)) {
      message("Skipping variable with no variance_imp information: ", v)
      return(NULL)
    }
    
    # BLIMP convention: nonzero finite entries indicate missing/imputed values
    miss_idx <- is.finite(vimp) & (vimp != 0)
    
    obs_vals <- model@imputations[[1]][[v]][!miss_idx]
    imp_vals <- unlist(lapply(model@imputations, function(d) d[[v]][miss_idx]), use.names = FALSE)
    
    if (!length(obs_vals) || all(is.na(obs_vals))) {
      message("Skipping variable with no observed data: ", v)
      return(NULL)
    }
    if (!length(imp_vals) || all(is.na(imp_vals))) {
      message("Skipping variable with no imputed data: ", v)
      return(NULL)
    }
    if (!is.numeric(obs_vals) || !is.numeric(imp_vals)) {
      message("Skipping non-numeric variable: ", v)
      return(NULL)
    }
    
    rng <- range(c(obs_vals, imp_vals), na.rm = TRUE)
    binwidth <- if (diff(rng) > 0) diff(rng) / bins else 1
    boundary <- rng[1]
    
    df_obs <- data.frame(x = obs_vals, grp = "Observed")
    df_imp <- data.frame(x = imp_vals, grp = rep("Imputed", length(imp_vals)))
    
    title_text <- if (is.null(main))
      paste0("Observed vs. Imputed Scores Over ", n_imps, " Imputed Data Sets: ", v)
    else main
    
    ggplot2::ggplot() +
      ggplot2::geom_histogram(
        data = df_obs,
        ggplot2::aes(x = x, y = ggplot2::after_stat(density), fill = grp),
        binwidth = binwidth, boundary = boundary,
        color = obs_col, alpha = plot_shading
      ) +
      ggplot2::geom_histogram(
        data = df_imp,
        ggplot2::aes(x = x, y = ggplot2::after_stat(density), color = grp),
        binwidth = binwidth, boundary = boundary,
        fill = NA, linewidth = 1.2
      ) +
      ggplot2::coord_cartesian(xlim = rng) +
      ggplot2::labs(
        title = title_text,
        x = v, y = NULL,
        fill = NULL, color = NULL
      ) +
      ggplot2::scale_fill_manual(values = c(Observed = obs_col), drop = TRUE) +
      ggplot2::scale_color_manual(values = c(Imputed = imp_col), drop = TRUE) +
      ggplot2::guides(
        fill  = ggplot2::guide_legend(order = 1, override.aes = list(color = obs_col, alpha = plot_shading)),
        color = ggplot2::guide_legend(order = 2, override.aes = list(fill = NA, linewidth = 1.2))
      ) +
      blimp_theme(font_size)
  }
  
  plots <- setNames(lapply(var, build_one), var)
  for (p in plots) if (!is.null(p)) print(p)
  invisible(plots)
}

# RESIDUALS VS. PREDICTED & RESIDUALS VS. PREDICTORS (+ INDEX, DISCRETE X FROM ORDINAL/NOMINAL) ----

residuals_plot <- function(
    model,
    var           = NULL,   # vector of DV bases (e.g., c("dpdd","inflam_sum"))
    # smoother & pooling
    smoother      = "loess",
    degree        = 4,
    span          = 0.9,
    robust        = TRUE,
    ci            = TRUE,
    level         = 0.95,
    # where to show the curve/ribbon
    support       = c("density", "quantile"),
    trim_prop     = 0.025,
    window_prop   = 0.10,
    min_n_prop    = 0.01,
    # robustification for FITTING ONLY
    center_by_imp = FALSE,
    winsor_fit    = TRUE,
    winsor_k      = 3,
    # display (view only)
    y_clip        = c("mad", "sd", "quantile", "none"),
    k             = 5,
    q_clip        = 0.005,
    # styling
    point_alpha   = 0.15,
    point_size    = 1.2,
    curve_color   = "violet",
    band_fill     = "violet",
    band_alpha    = plot_shading,   # follow global shading
    font_size     = 14,
    # index options
    show_index    = TRUE,
    signed_index  = TRUE,
    index_cutoff  = if (signed_index) c(-3, -2, 0, 2, 3) else c(2, 3),
    index_aggregate = c("mean", "max"),
    index_order   = c("rank", "row"),
    index_point_color = "teal",
    index_line_color  = "violet",
    print_threshold   = 3,
    print_head        = 20
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!is.list(model@imputations) || !length(model@imputations))
    stop("@imputations must be a non-empty list of data frames")
  if (!curve_color %in% names(plot_colors) || !band_fill %in% names(plot_colors))
    stop("curve_color and band_fill must be names in plot_colors: ",
         paste(names(plot_colors), collapse = ", "))
  
  support         <- match.arg(support)
  y_clip          <- match.arg(y_clip)
  index_aggregate <- match.arg(index_aggregate)
  index_order     <- match.arg(index_order)
  
  # ==== helpers ===============================================================
  
  # Rubin-pooled category means + CI (for discrete X)
  pool_means_by_category <- function(df_xy, level = 0.95) {
    levs <- levels(df_xy$x)
    out  <- lapply(levs, function(L) {
      dL <- df_xy[df_xy$x == L, , drop = FALSE]
      if (!nrow(dL)) return(NULL)
      imps <- split(dL, dL$imp)
      means <- Uj <- numeric(length(imps))
      for (j in seq_along(imps)) {
        yj <- imps[[j]]$y
        yj <- yj[is.finite(yj)]
        if (!length(yj)) { means[j] <- NA_real_; Uj[j] <- NA_real_; next }
        means[j] <- mean(yj)
        nj <- length(yj); sj2 <- stats::var(yj)
        Uj[j] <- if (is.finite(sj2) && nj > 0) sj2 / nj else NA_real_
      }
      means <- means[is.finite(means)]; Uj <- Uj[is.finite(Uj)]
      m <- length(means); if (!m) return(NULL)
      Qbar <- mean(means); Ubar <- if (length(Uj)) mean(Uj) else 0
      B <- if (m > 1) stats::var(means) else 0
      Tvar <- Ubar + (1 + 1/m) * B
      r <- if (Ubar > 0) ((1 + 1/m) * B) / Ubar else Inf
      nu <- (m - 1) * (1 + 1/r)^2; if (!is.finite(nu) || nu <= 0) nu <- m - 1
      tcrit <- stats::qt(1 - (1 - level)/2, df = max(1, nu))
      seTot <- sqrt(pmax(0, Tvar))
      data.frame(level = L, mean = Qbar,
                 lwr = Qbar - tcrit * seTot, upr = Qbar + tcrit * seTot,
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, out)
  }
  
  # view-only y-limits
  y_limits <- function(y) {
    switch(
      y_clip,
      "mad" = { m0 <- stats::median(y, na.rm = TRUE); md <- stats::mad(y, constant = 1.4826, na.rm = TRUE); c(m0 - k*md, m0 + k*md) },
      "sd"  = { mu <- mean(y, na.rm = TRUE); s  <- stats::sd(y,  na.rm = TRUE); c(mu - k*s,    mu + k*s)    },
      "quantile" = stats::quantile(y, c(q_clip, 1 - q_clip), na.rm = TRUE),
      "none" = range(y, na.rm = TRUE)
    )
  }
  
  # stack a y/x pair across imputations
  stack_cols <- function(ycol, xcol) {
    dfs <- lapply(seq_along(model@imputations), function(i) {
      d <- model@imputations[[i]]
      if (!all(c(ycol, xcol) %in% names(d))) return(NULL)
      data.frame(x = d[[xcol]], y = d[[ycol]], imp = i)
    })
    do.call(rbind, dfs)
  }
  
  # winsor for fitting only
  winsor_mad <- function(x, k = 3) {
    med <- stats::median(x, na.rm = TRUE)
    mad <- stats::mad(x, constant = 1.4826, na.rm = TRUE)
    if (!is.finite(mad) || mad == 0) return(x)
    lo <- med - k * mad; hi <- med + k * mad
    pmin(pmax(x, lo), hi)
  }
  
  # pooled loess
  loess_pooled <- function(df_xy, span, level, center_by_imp, winsor_fit, winsor_k, robust) {
    fitdat <- df_xy
    if (center_by_imp) {
      fitdat$y <- fitdat$y - ave(fitdat$y, fitdat$imp, FUN = function(z) mean(z, na.rm = TRUE))
    }
    if (winsor_fit) {
      fitdat$y <- ave(fitdat$y, fitdat$imp, FUN = function(z) winsor_mad(z, k = winsor_k))
    }
    rng_x <- range(fitdat$x, na.rm = TRUE)
    xgrid <- if (!is.finite(diff(rng_x)) || diff(rng_x) == 0) rng_x else seq(rng_x[1], rng_x[2], length.out = 200)
    imps  <- split(fitdat, fitdat$imp)
    fam   <- if (robust) "symmetric" else "gaussian"
    
    min_span_n <- 50L; min_span <- 0.12; min_unique_x <- 25L; jitter_frac <- 1e-7
    collapse_dupes <- function(d) {
      d <- stats::na.omit(d[, c("y","x")]); if (!nrow(d)) return(d)
      agg <- aggregate(y ~ x, data = d, FUN = mean); agg[order(agg$x), , drop = FALSE]
    }
    
    preds <- lapply(imps, function(d) {
      d0 <- collapse_dupes(d); n <- nrow(d0)
      if (n < 10) return(list(fit = rep(NA_real_, length(xgrid)), se2 = rep(NA_real_, length(xgrid))))
      u <- length(unique(d0$x))
      span_eff <- max(span, min_span, min_span_n / max(10L, n))
      if (u < min_unique_x) span_eff <- max(span_eff, min(1, min_unique_x / max(5L, u)))
      if (anyDuplicated(d0$x)) {
        xr <- diff(range(d0$x)); j <- if (is.finite(xr) && xr > 0) xr * jitter_frac else jitter_frac
        set.seed(1L); d0$x <- d0$x + stats::rnorm(n, 0, j)
      }
      ctl <- stats::loess.control(surface = "interpolate", trace.hat = "approximate")
      fit <- try(suppressWarnings(stats::loess(y ~ x, data = d0, span = min(1, span_eff), degree = 1,
                                               family = fam, control = ctl)), silent = TRUE)
      if (inherits(fit, "try-error")) return(list(fit = rep(NA_real_, length(xgrid)), se2 = rep(NA_real_, length(xgrid))))
      pr <- try(suppressWarnings(stats::predict(fit, newdata = data.frame(x = xgrid), se = TRUE)), silent = TRUE)
      if (inherits(pr, "try-error") || is.null(pr$se.fit)) {
        yhat <- try(suppressWarnings(stats::predict(fit, newdata = data.frame(x = xgrid))), silent = TRUE)
        yhat <- if (inherits(yhat, "try-error")) rep(NA_real_, length(xgrid)) else as.numeric(yhat)
        list(fit = yhat, se2 = rep(NA_real_, length(xgrid)))
      } else {
        list(fit = as.numeric(pr$fit), se2 = as.numeric(pr$se.fit)^2)
      }
    })
    
    Fmat <- do.call(cbind, lapply(preds, `[[`, "fit"))
    Wmat <- do.call(cbind, lapply(preds, `[[`, "se2"))
    keep <- which(colSums(is.finite(Fmat)) > 0)
    if (!length(keep)) return(data.frame(x = xgrid, mean = 0, lwr = 0, upr = 0))
    Fmat <- Fmat[, keep, drop = FALSE]; Wmat <- Wmat[, keep, drop = FALSE]
    m <- ncol(Fmat)
    
    mean_fit <- rowMeans(Fmat, na.rm = TRUE)
    W <- rowMeans(Wmat, na.rm = TRUE)
    B <- apply(Fmat, 1, stats::var, na.rm = TRUE); B[!is.finite(B)] <- 0
    Tvar <- W + (1 + 1/m) * B
    r <- ifelse(W > 0, ((1 + 1/m) * B) / W, Inf)
    nu <- (m - 1) * (1 + 1/r)^2; nu[!is.finite(nu) | nu <= 0] <- m - 1
    tcrit <- stats::qt(1 - (1 - level)/2, df = pmax(1, nu))
    seTot <- sqrt(pmax(0, Tvar))
    
    data.frame(x = xgrid, mean = mean_fit,
               lwr = mean_fit - tcrit * seTot,
               upr = mean_fit + tcrit * seTot)
  }
  
  # ==== detect available bases =================================================
  cols1 <- names(model@imputations[[1]])
  bases_from_cols <- unique(sub("\\.(residual|predicted)$", "",
                                grep("\\.(residual|predicted)$", cols1, value = TRUE)))
  has_resid_col   <- function(b) paste0(b, ".residual")   %in% cols1
  has_pair_cols   <- function(b) all(c(paste0(b, ".residual"), paste0(b, ".predicted")) %in% cols1)
  
  if (!is.null(var)) {
    stopifnot(is.character(var), length(var) >= 1)
    var_req <- unique(var)
  } else {
    var_req <- bases_from_cols
  }
  
  bases_resid <- var_req[vapply(var_req, has_resid_col, logical(1L))]
  bases_pair  <- var_req[vapply(var_req, has_pair_cols, logical(1L))]
  
  if (!is.null(var)) {
    if (length(setdiff(var_req, bases_resid)))
      message("Requested variable(s) missing '.residual' column; will skip where needed: ",
              paste(setdiff(var_req, bases_resid), collapse = ", "))
    if (length(setdiff(var_req, bases_pair)))
      message("Requested variable(s) missing '.predicted' (or residual) pair; skipping Residuals vs. Predicted: ",
              paste(setdiff(var_req, bases_pair), collapse = ", "))
  }
  
  # ==== categorical predictor set from @syntax (plus <4-unique fallback) =======
  cat_vars <- tolower(.get_categorical_vars(model))
  is_categorical_name <- function(vname) tolower(vname) %in% cat_vars
  
  # ==== colors, counts =========================================================
  curve_col <- unname(plot_colors[curve_color])
  band_col  <- unname(plot_colors[band_fill])
  n_imps    <- length(model@imputations)
  
  plots <- list()
  
  # A) Residuals vs. Predicted --------------------------------------------------
  if (length(bases_pair)) {
    build_rvp <- function(base) {
      ycol <- paste0(base, ".residual")
      xcol <- paste0(base, ".predicted")
      if (!all(c(ycol, xcol) %in% cols1)) { message("Skipping (columns missing): ", base); return(NULL) }
      df <- stack_cols(ycol, xcol)
      if (is.null(df) || !nrow(df)) { message("Skipping (no data across imputations): ", base); return(NULL) }
      if (!is.numeric(df$x) || !is.numeric(df$y)) { message("Skipping non-numeric: ", base); return(NULL) }
      
      if (tolower(smoother) == "loess") {
        curve_df <- loess_pooled(df, span, level, center_by_imp, winsor_fit, winsor_k, robust)
        if (support == "quantile") {
          pr <- stats::quantile(df$x, c(trim_prop, 1 - trim_prop), na.rm = TRUE)
          curve_df <- subset(curve_df, x >= pr[1] & x <= pr[2])
        } else {
          xr <- range(df$x, na.rm = TRUE); win <- window_prop * diff(xr)
          min_n <- max(10L, ceiling(min_n_prop * nrow(df)))
          counts <- vapply(curve_df$x, function(x0) sum(abs(df$x - x0) <= win/2), integer(1))
          curve_df <- curve_df[counts >= min_n, , drop = FALSE]
        }
      } else {
        rng_x <- range(df$x, na.rm = TRUE); xgrid <- seq(rng_x[1], rng_x[2], length.out = 200)
        imps  <- split(df, df$imp)
        coef_mat <- lapply(imps, function(d) {
          dd <- stats::na.omit(d[, c("y","x")])
          if (nrow(dd) < degree + 1) return(rep(NA_real_, degree + 1))
          fit <- try(stats::lm(y ~ stats::poly(x, degree = degree, raw = TRUE), data = dd), silent = TRUE)
          if (inherits(fit, "try-error")) return(rep(NA_real_, degree + 1))
          stats::coef(fit)
        })
        coef_mat <- do.call(rbind, coef_mat)
        coef_mat <- coef_mat[stats::complete.cases(coef_mat), , drop = FALSE]
        if (!nrow(coef_mat)) {
          curve_df <- data.frame(x = xgrid, mean = 0, lwr = NA, upr = NA)
        } else {
          beta <- colMeans(coef_mat)
          yhat <- beta[1] + sapply(xgrid, function(x) sum(beta[-1] * x^(seq_along(beta[-1]))))
          curve_df <- data.frame(x = xgrid, mean = as.numeric(yhat), lwr = NA, upr = NA)
        }
      }
      
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = point_alpha, size = point_size,
                            color = unname(plot_colors["teal"])) +
        ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
        { if (ci && "lwr" %in% names(curve_df) && nrow(curve_df))
          ggplot2::geom_ribbon(
            data = curve_df, ggplot2::aes(x = x, ymin = lwr, ymax = upr),
            inherit.aes = FALSE, fill = band_col, alpha = band_alpha
          ) else NULL } +
        ggplot2::geom_line(data = curve_df, ggplot2::aes(x = x, y = mean),
                           inherit.aes = FALSE, color = curve_col, linewidth = 1.2) +
        ggplot2::coord_cartesian(ylim = y_limits(df$y)) +
        ggplot2::labs(
          title = paste0("Residuals vs. Predicted Values Over ", n_imps,
                         " Imputed Data Sets: ", base),
          x = paste0(base, ".predicted"),
          y = paste0(base, ".residual")
        ) +
        blimp_theme(font_size) +
        ggplot2::theme(
          axis.text.y  = ggplot2::element_text(size = font_size),
          axis.ticks.y = ggplot2::element_line(),
          axis.text.x  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 2)),
          axis.title.x = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 12))
        )
    }
    rvp_plots <- setNames(lapply(bases_pair, build_rvp), paste0(bases_pair, "_vs_pred"))
    plots <- c(plots, rvp_plots)
    lapply(rvp_plots, function(p) if (!is.null(p)) print(p))
  }
  
  # B) Residuals vs. Predictors (from @MODEL) ----------------------------------
  model_lines <- .get_model_block(model)
  
  if (length(model_lines) && length(bases_resid)) {
    chunks <- unlist(strsplit(paste(model_lines, collapse = " "), ";", fixed = TRUE), use.names = FALSE)
    chunks <- gsub("\\s+", " ", trimws(chunks))
    chunks <- chunks[nzchar(chunks)]
    
    parse_rhs_vars <- function(rhs) {
      rhs2 <- strsplit(rhs, "\\|", perl = TRUE)[[1]][1]   # keep before vertical pipe
      toks <- strsplit(trimws(rhs2), "\\s+", perl = TRUE)[[1]]
      toks <- toks[nzchar(toks)]
      unique(toks)
    }
    
    # first build: DV name -> predictors
    dv_pred_pairs_raw <- list()
    for (ch in chunks) {
      if (!grepl("~", ch, fixed = TRUE)) next
      parts <- strsplit(ch, "~", fixed = TRUE)[[1]]
      dv_raw <- trimws(parts[1])
      rhs    <- trimws(parts[2])
      
      # handle prefixes like "focal: turnover" -> dv = "turnover"
      dv_tokens <- strsplit(dv_raw, "\\s+", perl = TRUE)[[1]]
      dv_token_last <- dv_tokens[length(dv_tokens)]
      dv <- sub(":$", "", dv_token_last)
      
      preds <- parse_rhs_vars(rhs)
      if (length(preds)) {
        dv_pred_pairs_raw[[dv]] <- unique(c(dv_pred_pairs_raw[[dv]], preds))
      }
    }
    
    if (length(dv_pred_pairs_raw)) {
      # map DV names (e.g. "drinker") to residual bases (e.g. "drinker.1")
      dv_pred_pairs <- list()
      for (dv in names(dv_pred_pairs_raw)) {
        preds <- dv_pred_pairs_raw[[dv]]
        # direct match (continuous outcomes etc.)
        if (dv %in% bases_resid) {
          dv_pred_pairs[[dv]] <- preds
        } else {
          # look for bases that start with "dv."
          cand <- bases_resid[startsWith(bases_resid, paste0(dv, "."))]
          if (length(cand)) {
            for (b in cand) {
              dv_pred_pairs[[b]] <- unique(c(dv_pred_pairs[[b]], preds))
            }
          }
        }
      }
      # keep only those with actual residual columns
      dv_pred_pairs <- dv_pred_pairs[names(dv_pred_pairs) %in% bases_resid]
    } else {
      dv_pred_pairs <- list()
    }
    
    if (length(dv_pred_pairs)) {
      build_rvx <- function(base, xname) {
        ycol <- paste0(base, ".residual"); xcol <- xname
        if (!ycol %in% cols1) { message("Skipping (residual missing): ", ycol); return(NULL) }
        if (!xcol %in% cols1) { message("Skipping (predictor missing): ", xcol); return(NULL) }
        df <- stack_cols(ycol, xcol)
        if (is.null(df) || !nrow(df)) { message("Skipping (no data across imputations): ", base, " ~ ", xname); return(NULL) }
        if (!is.numeric(df$y)) { message("Skipping non-numeric residuals for: ", base); return(NULL) }
        
        # categorical if: listed in ORDINAL/NOMINAL OR numeric with < 4 unique values
        unique_x <- length(unique(df$x[is.finite(df$x)]))
        is_cat <- is_categorical_name(xname) || (is.numeric(df$x) && unique_x > 0 && unique_x < 4)
        
        if (is_cat) {
          df$x_f <- factor(df$x)  # stable ticks at categories
          summ <- pool_means_by_category(
            df_xy = data.frame(y = df$y, x = df$x_f, imp = df$imp), level = level
          )
          if (!is.null(summ) && nrow(summ)) summ$x_f <- factor(summ$level, levels = levels(df$x_f))
          
          ggplot2::ggplot(df, ggplot2::aes(x = x_f, y = y)) +
            ggplot2::geom_jitter(width = 0.15, height = 0, alpha = point_alpha, size = point_size,
                                 color = unname(plot_colors["teal"])) +
            ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
            { if (!is.null(summ) && nrow(summ))
              ggplot2::geom_errorbar(
                data = summ, ggplot2::aes(x = x_f, ymin = lwr, ymax = upr),
                inherit.aes = FALSE, width = 0.18, color = unname(plot_colors["violet"]), alpha = 1
              ) else NULL } +
            { if (!is.null(summ) && nrow(summ))
              ggplot2::geom_point(
                data = summ, ggplot2::aes(x = x_f, y = mean),
                inherit.aes = FALSE, size = 2.6, color = unname(plot_colors["violet"])
              ) else NULL } +
            ggplot2::coord_cartesian(ylim = y_limits(df$y)) +
            ggplot2::labs(
              title = paste0("Residuals vs. Predictor Over ", n_imps,
                             " Imputed Data Sets: ", base, " vs. ", xname),
              x = xname,
              y = paste0(base, ".residual")
            ) +
            blimp_theme(font_size) +
            ggplot2::theme(
              axis.text.y  = ggplot2::element_text(size = font_size),
              axis.ticks.y = ggplot2::element_line(),
              axis.text.x  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 2)),
              axis.title.x = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 12))
            )
          
        } else {
          if (!is.numeric(df$x)) { message("Skipping non-numeric predictor: ", xname); return(NULL) }
          
          if (tolower(smoother) == "loess") {
            curve_df <- loess_pooled(df, span, level, center_by_imp, winsor_fit, winsor_k, robust)
            if (support == "quantile") {
              pr <- stats::quantile(df$x, c(trim_prop, 1 - trim_prop), na.rm = TRUE)
              curve_df <- subset(curve_df, x >= pr[1] & x <= pr[2])
            } else {
              xr <- range(df$x, na.rm = TRUE); win <- window_prop * diff(xr)
              min_n <- max(10L, ceiling(min_n_prop * nrow(df)))
              counts <- vapply(curve_df$x, function(x0) sum(abs(df$x - x0) <= win/2), integer(1))
              curve_df <- curve_df[counts >= min_n, , drop = FALSE]
            }
          } else {
            rng_x <- range(df$x, na.rm = TRUE); xgrid <- seq(rng_x[1], rng_x[2], length.out = 200)
            imps  <- split(df, df$imp)
            coef_mat <- lapply(imps, function(d) {
              dd <- stats::na.omit(d[, c("y","x")])
              if (nrow(dd) < degree + 1) return(rep(NA_real_, degree + 1))
              fit <- try(stats::lm(y ~ stats::poly(x, degree = degree, raw = TRUE), data = dd), silent = TRUE)
              if (inherits(fit, "try-error")) return(rep(NA_real_, degree + 1))
              stats::coef(fit)
            })
            coef_mat <- do.call(rbind, coef_mat)
            coef_mat <- coef_mat[stats::complete.cases(coef_mat), , drop = FALSE]
            if (!nrow(coef_mat)) {
              curve_df <- data.frame(x = xgrid, mean = 0, lwr = NA, upr = NA)
            } else {
              beta <- colMeans(coef_mat)
              yhat <- beta[1] + sapply(xgrid, function(x) sum(beta[-1] * x^(seq_along(beta[-1]))))
              curve_df <- data.frame(x = xgrid, mean = as.numeric(yhat), lwr = NA, upr = NA)
            }
          }
          
          ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_point(alpha = point_alpha, size = point_size,
                                color = unname(plot_colors["teal"])) +
            ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
            { if (ci && exists("curve_df") && "lwr" %in% names(curve_df) && nrow(curve_df))
              ggplot2::geom_ribbon(
                data = curve_df, ggplot2::aes(x = x, ymin = lwr, ymax = upr),
                inherit.aes = FALSE, fill = band_col, alpha = band_alpha
              ) else NULL } +
            { if (exists("curve_df"))
              ggplot2::geom_line(data = curve_df, ggplot2::aes(x = x, y = mean),
                                 inherit.aes = FALSE, color = curve_col, linewidth = 1.2) else NULL } +
            ggplot2::coord_cartesian(ylim = y_limits(df$y)) +
            ggplot2::labs(
              title = paste0("Residuals vs. Predictor Over ", n_imps,
                             " Imputed Data Sets: ", base, " vs. ", xname),
              x = xname,
              y = paste0(base, ".residual")
            ) +
            blimp_theme(font_size) +
            ggplot2::theme(
              axis.text.y  = ggplot2::element_text(size = font_size),
              axis.ticks.y = ggplot2::element_line(),
              axis.text.x  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 2)),
              axis.title.x = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 12))
            )
        }
      }
      
      rvx_list <- list()
      for (base in names(dv_pred_pairs)) {
        for (xname in dv_pred_pairs[[base]]) {
          p <- build_rvx(base, xname)
          nm <- paste0(base, "_vs_", xname)
          rvx_list[[nm]] <- p
          if (!is.null(p)) print(p)
        }
      }
      plots <- c(plots, rvx_list)
    }
  }
  
  # C) Standardized residual index plots + table --------------------------------
  if (isTRUE(show_index) && length(bases_resid)) {
    sr  <- standardize_residuals(model, vars = bases_resid)
    dat <- sr$data
    
    if (nrow(dat)) {
      n_sets <- length(model@imputations)
      
      split_idx <- interaction(dat$base, dat$row, drop = TRUE)
      if (signed_index) {
        z_case <- tapply(dat$z, split_idx, mean, na.rm = TRUE)
        yval   <- as.numeric(z_case)
        ylab   <- "Standardized Residual"
        keys   <- strsplit(names(z_case), split = ".", fixed = TRUE)
      } else {
        absz <- abs(dat$z)
        agg  <- if (index_aggregate == "mean") tapply(absz, split_idx, mean, na.rm = TRUE)
        else                                  tapply(absz, split_idx, max,  na.rm = TRUE)
        yval <- as.numeric(agg)
        ylab <- "Absolute Value of Standardized Residual"
        keys <- strsplit(names(tapply(dat$row, split_idx, length)), split = ".", fixed = TRUE)
      }
      base_i <- vapply(keys, `[`, character(1L), 1L)
      row_i  <- as.integer(vapply(keys, `[`, character(1L), 2L))
      df_idx <- data.frame(base = base_i, row = row_i, y = yval, stringsAsFactors = FALSE)
      
      bases_idx <- unique(dat$base)
      summaries <- lapply(bases_idx, function(b) {
        d <- dat[dat$base == b, , drop = FALSE]
        if (!nrow(d)) return(NULL)
        rows <- sort(unique(d$row))
        tab <- data.frame(
          variable    = b,
          row         = rows,
          num_outlier = NA_integer_,
          num_imps    = NA_integer_,
          mean_abs_z  = NA_real_,
          min_abs_z   = NA_real_,
          max_abs_z   = NA_real_,
          stringsAsFactors = FALSE
        )
        for (i in seq_along(rows)) {
          rr <- rows[i]
          zi <- d$z[d$row == rr]
          a  <- abs(zi)
          tab$num_outlier[i] <- sum(a > print_threshold, na.rm = TRUE)
          tab$num_imps[i]    <- sum(is.finite(a))
          tab$mean_abs_z[i]  <- mean(a, na.rm = TRUE)
          tab$min_abs_z[i]   <- suppressWarnings(min(a, na.rm = TRUE))
          tab$max_abs_z[i]   <- suppressWarnings(max(a, na.rm = TRUE))
        }
        tab <- tab[tab$num_outlier > 0, , drop = FALSE]
        if (!nrow(tab)) { message("No rows with |z| > ", print_threshold, " for ", b, "."); return(NULL) }
        tab[, c("mean_abs_z","min_abs_z","max_abs_z")] <- round(tab[, c("mean_abs_z","min_abs_z","max_abs_z")], 2)
        tab <- tab[order(-tab$num_outlier, -tab$mean_abs_z, tab$row), , drop = FALSE]
        message("Outlier summary for ", b, " (|z| > ", print_threshold, "): n rows = ", nrow(tab))
        print(utils::head(tab, print_head), row.names = FALSE)
        tab
      })
      names(summaries) <- bases_idx
      
      build_index <- function(b) {
        d <- df_idx[df_idx$base == b, , drop = FALSE]
        if (!nrow(d)) return(NULL)
        if (index_order == "rank") {
          d <- d[order(d$y), ]
          d$index <- seq_len(nrow(d))
          xtitle <- "Cases Ranked by Standardized Residual"
        } else {
          d$index <- d$row
          xtitle <- "Case (row order)"
        }
        cutoff_layer <- NULL
        if (length(index_cutoff)) {
          if (signed_index) {
            cutoff_layer <- list(
              ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.8),
              ggplot2::geom_hline(
                yintercept = index_cutoff[index_cutoff != 0],
                color = unname(plot_colors[index_line_color]),
                linewidth = 0.7, linetype = "dashed"
              )
            )
          } else {
            cutoff_layer <- list(
              ggplot2::geom_hline(
                yintercept = index_cutoff,
                color = unname(plot_colors[index_line_color]),
                linewidth = 0.7, linetype = "dashed"
              )
            )
          }
        }
        ggplot2::ggplot(d, ggplot2::aes(x = index, y = y)) +
          ggplot2::geom_point(size = 1.4, alpha = 0.85, color = unname(plot_colors[index_point_color])) +
          cutoff_layer +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
          ggplot2::labs(
            title = paste0("Standardized Residual Index Plot Over ", n_sets,
                           " Imputed Data Sets: ", b),
            x = xtitle,
            y = "Standardized Residual"
          ) +
          blimp_theme(font_size) +
          ggplot2::theme(
            axis.text.y  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(r = 6)),
            axis.ticks.y = ggplot2::element_line(),
            axis.title.y = ggplot2::element_text(size = font_size),
            axis.text.x  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 2)),
            axis.title.x = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 12)),
            panel.border = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
          )
      }
      
      idx_plots <- setNames(lapply(bases_idx, build_index), paste0(bases_idx, "_index"))
      plots <- c(plots, idx_plots)
      lapply(idx_plots, function(p) if (!is.null(p)) print(p))
      
      return(invisible(list(plots = plots, summaries = summaries)))
    } else {
      message("No standardized residuals available for index plots.")
      return(invisible(plots))
    }
  }
  
  invisible(plots)
}

# BIVARIATE SCATTER WITH LOESS ----
# Adds proper y-axis labeling, tick marks, and bounds for probabilities

bivariate_plot <- function(
    model, formula,
    span = 0.9, degree = 1, robust = TRUE, level = 0.95,
    support = c("density", "quantile"),
    trim_prop = 0.025, window_prop = 0.10, min_n_prop = 0.01,
    point_alpha = 0.15, point_size = 1.2,
    curve_color = "violet", band_fill = "violet",
    font_size = 14
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  stopifnot(inherits(formula, "formula"))
  support <- match.arg(support)
  
  # --- parse y ~ x
  tt <- terms(formula)
  vars <- all.vars(tt)
  if (length(vars) != 2L) stop("Formula must be like y ~ x.")
  y_name <- vars[1]; x_name <- vars[2]
  
  # --- checks & stack
  if (!is.list(model@imputations) || !length(model@imputations))
    stop("@imputations must be a non-empty list of data frames")
  cols1 <- names(model@imputations[[1]])
  if (!all(c(y_name, x_name) %in% cols1))
    stop("Both '", y_name, "' and '", x_name, "' must exist in @imputations data.")
  
  df <- do.call(rbind, lapply(seq_along(model@imputations), function(i) {
    d <- model@imputations[[i]]
    data.frame(x = d[[x_name]], y = d[[y_name]], imp = i)
  }))
  df <- stats::na.omit(df[, c("x","y","imp")])
  if (!nrow(df)) stop("No complete cases for the selected variables.")
  if (!is.numeric(df$x) || !is.numeric(df$y)) stop("Both variables must be numeric.")
  n_imps <- length(model@imputations)
  
  # --- detect probability-like outcome
  qy <- stats::quantile(df$y, c(0.01, 0.99), na.rm = TRUE)
  use_logit <- is.finite(qy[1]) && is.finite(qy[2]) && qy[1] >= 0 && qy[2] <= 1
  inv_logit <- function(z) 1/(1+exp(-z))
  clamp01 <- function(v, eps = 1e-6) pmin(pmax(v, eps), 1 - eps)
  
  # --- decide discrete vs continuous x
  ux <- sort(unique(df$x[is.finite(df$x)]))
  is_discrete <- length(ux) <= 3
  
  # --- helpers
  winsor_mad <- function(x, k = 3) {
    med <- stats::median(x, na.rm = TRUE)
    mad <- stats::mad(x, constant = 1.4826, na.rm = TRUE)
    if (!is.finite(mad) || mad == 0) return(x)
    pmin(pmax(x, med - k*mad), med + k*mad)
  }
  
  # --- Rubin pooling of LOESS curve (continuous x)
  loess_pooled <- function(dat_xy) {
    fitdat <- dat_xy
    fam <- if (robust) "symmetric" else "gaussian"
    if (use_logit) {
      fitdat$y <- clamp01(fitdat$y)
      fitdat$y <- qlogis(fitdat$y)
    } else {
      fitdat$y <- ave(fitdat$y, fitdat$imp, FUN = function(z) winsor_mad(z, k = 3))
    }
    rng_x <- range(fitdat$x, na.rm = TRUE)
    xgrid <- seq(rng_x[1], rng_x[2], length.out = 200)
    imps  <- split(fitdat, fitdat$imp)
    
    preds <- lapply(imps, function(d) {
      d0 <- stats::na.omit(d[, c("y","x")])
      if (nrow(d0) < 10) return(list(fit = rep(NA_real_, length(xgrid)), se2 = rep(NA_real_, length(xgrid))))
      ctl <- stats::loess.control(surface = "interpolate", trace.hat = "approximate")
      fit <- try(suppressWarnings(stats::loess(y ~ x, data = d0, span = span, degree = degree, family = fam, control = ctl)), silent = TRUE)
      if (inherits(fit, "try-error")) return(list(fit = rep(NA_real_, length(xgrid)), se2 = rep(NA_real_, length(xgrid))))
      pr <- try(suppressWarnings(stats::predict(fit, newdata = data.frame(x = xgrid), se = TRUE)), silent = TRUE)
      if (inherits(pr, "try-error") || is.null(pr$se.fit))
        return(list(fit = as.numeric(stats::predict(fit, newdata = data.frame(x = xgrid))), se2 = rep(NA_real_, length(xgrid))))
      list(fit = as.numeric(pr$fit), se2 = as.numeric(pr$se.fit)^2)
    })
    
    Fmat <- do.call(cbind, lapply(preds, `[[`, "fit"))
    Wmat <- do.call(cbind, lapply(preds, `[[`, "se2"))
    keep <- which(colSums(is.finite(Fmat)) > 0)
    if (!length(keep)) return(data.frame(x = xgrid, mean = NA, lwr = NA, upr = NA))
    Fmat <- Fmat[, keep, drop = FALSE]; Wmat <- Wmat[, keep, drop = FALSE]
    m <- ncol(Fmat)
    
    mean_fit <- rowMeans(Fmat, na.rm = TRUE)
    W <- rowMeans(Wmat, na.rm = TRUE)
    B <- apply(Fmat, 1, stats::var, na.rm = TRUE); B[!is.finite(B)] <- 0
    Tvar <- W + (1 + 1/m) * B
    tcrit <- stats::qt(1 - (1 - level)/2, df = pmax(1, m - 1))
    seTot <- sqrt(pmax(0, Tvar))
    
    if (use_logit) {
      mu  <- inv_logit(mean_fit)
      lwr <- inv_logit(mean_fit - tcrit * seTot)
      upr <- inv_logit(mean_fit + tcrit * seTot)
    } else {
      mu  <- mean_fit
      lwr <- mean_fit - tcrit * seTot
      upr <- mean_fit + tcrit * seTot
    }
    data.frame(x = xgrid, mean = mu, lwr = lwr, upr = upr)
  }
  
  # --- Rubin-pooled mean per x-level (discrete x)
  pooled_mean_by_level <- function(dat_xy) {
    if (use_logit) {
      dat_xy$y <- clamp01(dat_xy$y)
      dat_xy$y <- qlogis(dat_xy$y)
    }
    levs <- sort(unique(dat_xy$x))
    m <- length(unique(dat_xy$imp))
    out <- lapply(levs, function(L) {
      by_imp <- split(dat_xy[dat_xy$x == L, , drop = FALSE], dat_xy$imp[dat_xy$x == L])
      mu_j <- vapply(by_imp, function(d) mean(d$y), numeric(1L))
      n_j  <- vapply(by_imp, nrow, integer(1L))
      s2_j <- vapply(by_imp, function(d) stats::var(d$y), numeric(1L))
      Ubar <- mean(s2_j / pmax(1, n_j))
      B <- stats::var(mu_j)
      Tvar <- Ubar + (1 + 1/m) * B
      se <- sqrt(pmax(0, Tvar))
      tcrit <- stats::qt(1 - (1 - level)/2, df = pmax(1, m - 1))
      if (use_logit) {
        c(mean = inv_logit(mean(mu_j)), lwr = inv_logit(mean(mu_j) - tcrit * se), upr = inv_logit(mean(mu_j) + tcrit * se))
      } else {
        c(mean = mean(mu_j), lwr = mean(mu_j) - tcrit * se, upr = mean(mu_j) + tcrit * se)
      }
    })
    out <- do.call(rbind, out)
    data.frame(x = levs, mean = out[, "mean"], lwr = out[, "lwr"], upr = out[, "upr"])
  }
  
  # --- plotting setup
  y_limits <- if (use_logit) c(0, 1) else range(df$y, na.rm = TRUE)
  y_breaks <- scales::pretty_breaks()(y_limits)
  
  if (is_discrete) {
    mean_df <- pooled_mean_by_level(df)
    ggplot2::ggplot(df, ggplot2::aes(x = factor(x), y = y)) +
      ggplot2::geom_jitter(width = 0.08, alpha = point_alpha, size = point_size,
                           color = unname(plot_colors["teal"])) +
      ggplot2::geom_errorbar(
        data = mean_df,
        ggplot2::aes(x = factor(x), ymin = lwr, ymax = upr),
        inherit.aes = FALSE,
        width = 0.15,
        color = unname(plot_colors[curve_color]),
        linewidth = 0.9
      ) +
      ggplot2::geom_point(
        data = mean_df,
        ggplot2::aes(x = factor(x), y = mean),
        inherit.aes = FALSE,
        size = 2.2,
        color = unname(plot_colors[curve_color])
      ) +
      ggplot2::scale_y_continuous(breaks = y_breaks, limits = y_limits) +
      ggplot2::labs(
        title = paste0("Bivariate Plot Over ", n_imps, " Imputed Data Sets: ",
                       y_name, " vs. ", x_name),
        x = x_name, y = y_name
      ) +
      blimp_theme(font_size) +
      ggplot2::theme(
        axis.text.y  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(r = 6)),
        axis.ticks.y = ggplot2::element_line(),
        axis.title.y = ggplot2::element_text(size = font_size),
        axis.text.x  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 2)),
        axis.title.x = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 12))
      )
  } else {
    curve_df <- loess_pooled(df)
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = point_alpha, size = point_size,
                          color = unname(plot_colors["teal"])) +
      ggplot2::geom_ribbon(
        data = curve_df, ggplot2::aes(x = x, ymin = lwr, ymax = upr),
        inherit.aes = FALSE, fill = unname(plot_colors[band_fill]), alpha = plot_shading
      ) +
      ggplot2::geom_line(
        data = curve_df, ggplot2::aes(x = x, y = mean),
        inherit.aes = FALSE, color = unname(plot_colors[curve_color]), linewidth = 1.2
      ) +
      ggplot2::scale_y_continuous(breaks = y_breaks, limits = y_limits) +
      ggplot2::labs(
        title = paste0("Bivariate Plot with LOESS Over ", n_imps,
                       " Imputed Data Sets: ", y_name, " vs. ", x_name),
        x = x_name, y = y_name
      ) +
      blimp_theme(font_size) +
      ggplot2::theme(
        axis.text.y  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(r = 6)),
        axis.ticks.y = ggplot2::element_line(),
        axis.title.y = ggplot2::element_text(size = font_size),
        axis.text.x  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 2)),
        axis.title.x = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 12))
      )
  }
}