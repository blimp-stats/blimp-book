# PLOT IMPUTATIONS OVERLAY HISTOGRAM ----

plot_imputations <- function(model, raw_df, var, bins = 50, main = NULL) {
  stopifnot(is.character(var), length(var) == 1)
  if (!var %in% names(raw_df)) stop("Variable not found in raw data: ", var)
  if (!is.list(model@imputations) || length(model@imputations) == 0)
    stop("@imputations must be a non-empty list of data frames")
  
  # 1) Missingness indicator
  miss_idx <- is.na(raw_df[[var]])
  
  # 2) Stack imputations and extract only imputed rows
  imps_list <- model@imputations
  imp_vals <- unlist(lapply(imps_list, function(d) d[[var]][miss_idx]), use.names = FALSE)
  obs_vals <- raw_df[[var]][!miss_idx]
  if (!is.numeric(obs_vals) || !is.numeric(imp_vals))
    stop("Variable must be numeric to plot histogram")
  
  rng <- range(c(obs_vals, imp_vals), na.rm = TRUE)
  if (is.null(main)) main <- paste("Observed vs Imputed Data:", var)
  breaks <- seq(rng[1], rng[2], length.out = bins + 1)
  
  # Observed (filled grey)
  hist(obs_vals, breaks = breaks, freq = FALSE,
       col = "grey70", border = "grey70",
       xlim = rng, main = main, xlab = var)
  
  # Imputed (blue outline)
  hist(imp_vals, breaks = breaks, freq = FALSE,
       add = TRUE, col = NA, border = "blue", lwd = 2)
  
  par(xpd = NA)  # allow drawing into figure region, not clipped by plot
  legend("topright",
         inset = c(-0.02, -0.02),   # small negative inset to push outward
         legend = c("Observed", "Imputed"),
         fill   = c("grey70", NA),
         border = c("grey70", "blue"),
         bty    = "n",
         lty    = 0, lwd = 0,
         text.col = "black",
         xpd = NA)
  par(xpd = FALSE)
}