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

# alpha used for filled layers (e.g., observed histogram fill)
plot_shading <- 0.25

# shared ggplot theme for all diagnostics
blimp_theme <- function(font_size = 14) {
  ggplot2::theme_minimal(base_size = font_size) %+replace%
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y  = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 2)),
      axis.title.x = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 12)),
      plot.title   = ggplot2::element_text(hjust = 0.5, face = "bold", size = font_size + 2),
      legend.title = ggplot2::element_text(size = font_size),
      legend.text  = ggplot2::element_text(size = font_size),
      legend.key.height = ggplot2::unit(1.1, "lines"),
      legend.key.width  = ggplot2::unit(1.6, "lines"),
      legend.position = c(0.95, 0.85),
      legend.justification = c("right", "top"),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )
}

# extract variable names referenced in the MODEL section of the blimp syntax
.extract_model_vars <- function(model) {
  # ensure @syntax exists
  if (is.null(model@syntax)) stop("@syntax not found on model object")
  
  syntax_obj <- model@syntax
  
  # case 1: blimp_syntax/list with a 'model' element (modern objects)
  if ((inherits(syntax_obj, "blimp_syntax") || is.list(syntax_obj)) &&
      "model" %in% names(syntax_obj)) {
    model_text <- syntax_obj$model
  } else {
    # case 2: fallback to text parsing (legacy/other forms)
    syntax_text  <- as.character(syntax_obj)
    syntax_lines <- unlist(strsplit(paste(syntax_text, collapse = "\n"), "\n"))
    model_lines  <- grep("^\\s*MODEL\\s*:", syntax_lines, value = TRUE)
    if (!length(model_lines)) stop("No MODEL section found in @syntax")
    model_text <- paste(model_lines, collapse = " ")
  }
  
  # parse variables after '~' and before ';'
  model_rhs <- sub(".*~", "", model_text)
  model_rhs <- gsub(";", "", model_rhs)
  vars <- strsplit(model_rhs, "\\s+")[[1]]
  vars <- vars[nzchar(vars)]
  unique(vars)
}


# IMPUTATION HISTOGRAM(S) ----

plot_imputations <- function(model, var = NULL, bins = 50, main = NULL,
                             fill_color = "teal", font_size = 14) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  # basic checks
  if (!is.list(model@imputations) || !length(model@imputations)) stop("@imputations must be non-empty")
  if (!fill_color %in% names(plot_colors)) stop("fill_color must be one of: ", paste(names(plot_colors), collapse = ", "))
  
  # available variables from first completed data set
  vars_all <- names(model@imputations[[1]])
  
  # derive variable list if var = NULL (based on MODEL variables + suffix matches)
  if (is.null(var)) {
    model_vars <- .extract_model_vars(model)
    var <- vars_all[vapply(
      vars_all,
      function(v) any(vapply(model_vars, grepl, logical(1), x = v, fixed = TRUE)),
      logical(1)
    )]
    if (!length(var)) stop("No matching variables found based on MODEL section.")
    message("Plotting variables based on MODEL section: ", paste(var, collapse = ", "))
  } else if (!all(var %in% vars_all)) {
    bad <- setdiff(var, vars_all)
    stop("Variable(s) not found in imputations: ", paste(bad, collapse = ", "))
  }
  
  fill_col <- unname(plot_colors[fill_color])
  n_sets   <- length(model@imputations)
  
  build_one <- function(v) {
    # stack all imputed values across sets
    imp_vals <- unlist(lapply(model@imputations, `[[`, v), use.names = FALSE)
    if (!is.numeric(imp_vals)) { message("Skipping non-numeric variable: ", v); return(NULL) }
    
    # default title if not specified
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

plot_imputed_vs_observed <- function(model, var = NULL, bins = 50, main = NULL,
                                     observed_fill = "teal", imputed_line = "violet",
                                     font_size = 14) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  # basic checks
  if (!is.list(model@imputations) || !length(model@imputations)) stop("@imputations must be non-empty")
  if (is.null(model@variance_imp)) stop("@variance_imp not found on model object")
  if (!observed_fill %in% names(plot_colors) || !imputed_line %in% names(plot_colors))
    stop("observed_fill and imputed_line must be names in plot_colors: ",
         paste(names(plot_colors), collapse = ", "))
  
  # candidate variables: present in both variance_imp and imputed data
  vars_all <- intersect(names(model@variance_imp), names(model@imputations[[1]]))
  
  # derive variable list if var = NULL (MODEL variables + suffix matches)
  if (is.null(var)) {
    model_vars <- .extract_model_vars(model)
    var <- vars_all[vapply(
      vars_all,
      function(v) any(vapply(model_vars, grepl, logical(1), x = v, fixed = TRUE)),
      logical(1)
    )]
    if (!length(var)) stop("No matching variables found based on MODEL section.")
    message("Plotting variables with observed values from MODEL section: ", paste(var, collapse = ", "))
  }
  
  obs_col <- unname(plot_colors[observed_fill])
  imp_col <- unname(plot_colors[imputed_line])
  
  build_one <- function(v) {
    vimp <- model@variance_imp[[v]]
    miss_idx <- is.finite(vimp) & (vimp != 0)
    
    obs_vals <- model@imputations[[1]][[v]][!miss_idx]
    imp_vals <- unlist(lapply(model@imputations, function(d) d[[v]][miss_idx]), use.names = FALSE)
    
    # skip if no observed data
    if (!length(obs_vals) || all(is.na(obs_vals))) {
      message("Skipping variable with no observed data: ", v)
      return(NULL)
    }
    
    # skip if no imputed data
    if (!length(imp_vals) || all(is.na(imp_vals))) {
      message("Skipping variable with no imputed data: ", v)
      return(NULL)
    }
    
    # ensure numeric
    if (!is.numeric(obs_vals) || !is.numeric(imp_vals)) {
      message("Skipping non-numeric variable: ", v)
      return(NULL)
    }
    
    rng <- range(c(obs_vals, imp_vals), na.rm = TRUE)
    binwidth <- if (diff(rng) > 0) diff(rng) / bins else 1
    boundary <- rng[1]
    
    df_obs <- data.frame(x = obs_vals, grp = "Observed")
    df_imp <- data.frame(x = imp_vals, grp = rep("Imputed", length(imp_vals)))
    title_text <- if (is.null(main)) paste("Observed vs Imputed Data:", v) else main
    
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
      ggplot2::labs(title = title_text, x = v, y = NULL, fill = NULL, color = NULL) +
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

# RESIDUALS VS. PREDICTED ----
# Rubin-pooled LOESS with robust options and support-aware trimming

plot_residuals <- function(
    model,
    smoother      = "loess",     # "loess" (default) or "poly"
    degree        = 4,           # if smoother = "poly"
    span          = 0.9,         # loess span (a bit larger for stability)
    robust        = TRUE,        # loess family = "symmetric" if TRUE
    ci            = TRUE,        # draw Rubin-pooled CI band for loess
    level         = 0.95,        # CI level
    
    # where to show the curve/ribbon
    support       = c("density", "quantile"),
    trim_prop     = 0.025,       # used if support = "quantile"
    window_prop   = 0.10,        # used if support = "density" (window width = this * x-range)
    min_n_prop    = 0.01,        # used if support = "density" (min fraction of N inside the window)
    
    # robustification for FITTING ONLY (points are unchanged)
    center_by_imp = FALSE,       # <- keep FALSE for this diagnostic
    winsor_fit    = TRUE,        # winsorize residuals for fitting only
    winsor_k      = 3,           # ±k * MAD for winsorization
    
    # display options (do not alter data; only the view)
    y_clip        = c("mad", "sd", "quantile", "none"),
    k             = 5,           # multiplier for MAD/SD display limits
    q_clip        = 0.005,       # tail prob for quantile clipping (both tails)
    
    point_alpha   = 0.15,
    point_size    = 1.2,
    curve_color   = "violet",    # names in plot_colors
    band_fill     = "violet",
    band_alpha    = 0.15,
    font_size     = 14
) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")
  if (!is.list(model@imputations) || !length(model@imputations))
    stop("@imputations must be a non-empty list of data frames")
  if (!curve_color %in% names(plot_colors) || !band_fill %in% names(plot_colors))
    stop("curve_color and band_fill must be names in plot_colors: ",
         paste(names(plot_colors), collapse = ", "))
  
  support <- match.arg(support)
  y_clip  <- match.arg(y_clip)
  
  # --- find *.residual / *.predicted bases in the first imputation
  cols1  <- names(model@imputations[[1]])
  bases  <- unique(sub("\\.(residual|predicted)$", "", grep("\\.(residual|predicted)$", cols1, value = TRUE)))
  has_pair <- vapply(bases, function(b)
    all(c(paste0(b, ".residual"), paste0(b, ".predicted")) %in% cols1), logical(1L))
  bases <- bases[has_pair]
  if (!length(bases)) { message("No *.residual / *.predicted pairs found."); return(invisible(list())) }
  
  # --- helper: stack residual/predicted across imputations
  stack_pairs <- function(base) {
    r <- paste0(base, ".residual"); p <- paste0(base, ".predicted")
    dfs <- lapply(seq_along(model@imputations), function(i) {
      d <- model@imputations[[i]]
      if (!all(c(r, p) %in% names(d))) return(NULL)
      data.frame(pred = d[[p]], resid = d[[r]], imp = i)
    })
    do.call(rbind, dfs)
  }
  
  # --- helper: winsorize by MAD (for fitting only)
  winsor_mad <- function(x, k = 3) {
    med <- stats::median(x, na.rm = TRUE)
    mad <- stats::mad(x, constant = 1.4826, na.rm = TRUE)
    if (!is.finite(mad) || mad == 0) return(x)
    lo <- med - k * mad; hi <- med + k * mad
    pmin(pmax(x, lo), hi)
  }
  
  # --- pooled loess (degree = 1) with Rubin-pooled CI at a common x-grid
  loess_pooled <- function(df, span, level, center_by_imp, winsor_fit, winsor_k, robust) {
    # prepare fitting copy (optionally robustify residuals for fitting ONLY)
    df_fit <- df
    if (center_by_imp) {
      df_fit$resid <- df_fit$resid - ave(df_fit$resid, df_fit$imp, FUN = function(z) mean(z, na.rm = TRUE))
    }
    if (winsor_fit) {
      df_fit$resid <- ave(df_fit$resid, df_fit$imp, FUN = function(z) winsor_mad(z, k = winsor_k))
    }
    
    rng_x <- range(df_fit$pred, na.rm = TRUE)
    xgrid <- if (!is.finite(diff(rng_x)) || diff(rng_x) == 0) rng_x else seq(rng_x[1], rng_x[2], length.out = 200)
    imps  <- split(df_fit, df_fit$imp)
    fam   <- if (robust) "symmetric" else "gaussian"
    
    preds <- lapply(imps, function(d) {
      dd <- stats::na.omit(d[, c("resid", "pred")])
      if (nrow(dd) < 10) return(list(fit = rep(NA_real_, length(xgrid)), se2 = rep(NA_real_, length(xgrid))))
      fit <- try(stats::loess(resid ~ pred, data = dd, span = span, degree = 1,
                              family = fam, control = stats::loess.control(surface = "direct")), silent = TRUE)
      if (inherits(fit, "try-error")) return(list(fit = rep(NA_real_, length(xgrid)), se2 = rep(NA_real_, length(xgrid))))
      pr  <- try(stats::predict(fit, newdata = data.frame(pred = xgrid), se = TRUE), silent = TRUE)
      if (inherits(pr, "try-error") || is.null(pr$se.fit)) {
        yhat <- try(stats::predict(fit, newdata = data.frame(pred = xgrid)), silent = TRUE)
        return(list(fit = if (inherits(yhat, "try-error")) rep(NA_real_, length(xgrid)) else as.numeric(yhat),
                    se2 = rep(NA_real_, length(xgrid))))
      } else {
        return(list(fit = as.numeric(pr$fit), se2 = as.numeric(pr$se.fit)^2))
      }
    })
    
    Fmat <- do.call(cbind, lapply(preds, `[[`, "fit"))
    Wmat <- do.call(cbind, lapply(preds, `[[`, "se2"))
    keep <- which(colSums(is.finite(Fmat)) > 0)
    if (!length(keep)) return(data.frame(x = xgrid, mean = 0, lwr = 0, upr = 0))
    Fmat <- Fmat[, keep, drop = FALSE]
    Wmat <- Wmat[, keep, drop = FALSE]
    m    <- ncol(Fmat)
    
    mean_fit <- rowMeans(Fmat, na.rm = TRUE)
    W <- rowMeans(Wmat, na.rm = TRUE)                       # within
    B <- apply(Fmat, 1, stats::var, na.rm = TRUE); B[!is.finite(B)] <- 0  # between
    Tvar <- W + (1 + 1/m) * B                               # total
    r  <- ifelse(W > 0, ((1 + 1/m) * B) / W, Inf)
    nu <- (m - 1) * (1 + 1/r)^2; nu[!is.finite(nu) | nu <= 0] <- m - 1
    tcrit <- stats::qt(1 - (1 - level)/2, df = pmax(1, nu))
    seTot <- sqrt(pmax(0, Tvar))
    
    data.frame(x = xgrid, mean = mean_fit,
               lwr = mean_fit - tcrit * seTot,
               upr = mean_fit + tcrit * seTot)
  }
  
  curve_col <- unname(plot_colors[curve_color])
  band_col  <- unname(plot_colors[band_fill])
  n_imps    <- length(model@imputations)
  
  # --- build plot per base variable
  build_one <- function(base) {
    df <- stack_pairs(base)
    if (is.null(df) || !nrow(df)) { message("Skipping (no data across imputations): ", base); return(NULL) }
    if (!is.numeric(df$pred) || !is.numeric(df$resid)) { message("Skipping non-numeric residual/predicted pair: ", base); return(NULL) }
    
    # smoother
    if (tolower(smoother) == "loess") {
      curve_df <- loess_pooled(df, span, level, center_by_imp, winsor_fit, winsor_k, robust)
      
      # support-aware trimming
      if (support == "quantile") {
        pr <- stats::quantile(df$pred, c(trim_prop, 1 - trim_prop), na.rm = TRUE)
        curve_df <- subset(curve_df, x >= pr[1] & x <= pr[2])
      } else {
        xr <- range(df$pred, na.rm = TRUE); win <- window_prop * diff(xr)
        min_n <- max(10L, ceiling(min_n_prop * nrow(df)))
        counts <- vapply(curve_df$x, function(x0) sum(abs(df$pred - x0) <= win/2), integer(1))
        curve_df <- curve_df[counts >= min_n, , drop = FALSE]
      }
      
    } else {
      # polynomial (no MI CI here)
      rng_x <- range(df$pred, na.rm = TRUE)
      xgrid <- seq(rng_x[1], rng_x[2], length.out = 200)
      imps  <- split(df, df$imp)
      coef_mat <- lapply(imps, function(d) {
        dd <- stats::na.omit(d[, c("resid","pred")])
        if (nrow(dd) < degree + 1) return(rep(NA_real_, degree + 1))
        fit <- try(stats::lm(resid ~ stats::poly(pred, degree = degree, raw = TRUE), data = dd), silent = TRUE)
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
    
    # robust display y-limits (view only)
    ylim <- switch(
      y_clip,
      "mad" = {
        m0 <- stats::median(df$resid, na.rm = TRUE)
        md <- stats::mad(df$resid, constant = 1.4826, na.rm = TRUE)
        c(m0 - k * md, m0 + k * md)
      },
      "sd" = {
        mu <- mean(df$resid, na.rm = TRUE); s <- stats::sd(df$resid, na.rm = TRUE)
        c(mu - k * s, mu + k * s)
      },
      "quantile" = stats::quantile(df$resid, c(q_clip, 1 - q_clip), na.rm = TRUE),
      "none" = range(df$resid, na.rm = TRUE)
    )
    
    ggplot2::ggplot(df, ggplot2::aes(x = pred, y = resid)) +
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
      ggplot2::coord_cartesian(ylim = ylim) +
      ggplot2::labs(
        title = paste0("Residuals vs. Predicted Values Over ", length(model@imputations),
                       " Imputed Data Sets: ", base),
        x = paste0(base, ".predicted"),
        y = paste0(base, ".residual")
      ) +
      blimp_theme(font_size) +
      ggplot2::theme(
        axis.text.y  = ggplot2::element_text(size = font_size),
        axis.ticks.y = ggplot2::element_line()
      )
  }
  
  plots <- setNames(lapply(bases, build_one), bases)
  for (p in plots) if (!is.null(p)) print(p)
  invisible(plots)
}