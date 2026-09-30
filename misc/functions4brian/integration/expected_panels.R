# =============================================================================
# expected_panels.R -- the panel manifest + checker for the real-Blimp
# integration pass. Sourced by integration_checks.R.
#
# For each model family, EXPECT records:
#   must_have     panel names that MUST be present (proves the branch fired)
#   must_have_rx  regexes that must each match >= 1 panel (family-level checks)
#   must_not      regexes that must match NO panel (suppression decisions)
# These encode the Design Contract at the level a user sees (the returned list
# names), so a real Blimp run either confirms the contract or fails loudly.
# =============================================================================

EXPECT <- list(

  # continuous single-level (Ch2 2.1, inflammation.csv): full linear suite,
  # leverage/Cook's present, no clustering artifacts.
  continuous = list(
    must_have    = c("dpdd.predicted", "dpdd.inflam", "dpdd.index",
                     "dpdd.leverage", "dpdd.cooks"),
    must_have_rx = character(0),
    must_not     = c("cluster_variance", "\\[.*\\]")
  ),

  # binary nominal (Ch3 3.2, alcohol.csv): calibration + overlays; NO linear
  # influence/index panels.
  binary = list(
    must_have    = c("drinker.1.binned"),
    must_have_rx = c("\\.probability$"),            # predicted-probability overlay(s)
    must_not     = c("\\.cooks$", "\\.leverage$", "\\.index$", "cluster_variance")
  ),

  # MLM random intercept (Ch7 7.2, mathprobsolve.csv): level-2 residual panels
  # + within-cluster spread; OLS influence suppressed under clustering.
  mlm = list(
    must_have    = c("probsolve.predicted", "probsolve.cluster_variance",
                     "probsolve[school].index"),
    must_have_rx = c("^probsolve\\[school\\]\\."),  # level-2 diagnostics exist
    must_not     = c("\\.cooks$", "\\.leverage$")
  ),

  # MLM-SEM named latents (Ch7 7.6, diary.csv): b0c/b1c routed to level-2;
  # posaff (level-1 DV) gets the heterogeneity panel; no OLS influence.
  mlm_sem = list(
    must_have    = c("b0c.index", "b1c.index", "posaff.cluster_variance",
                     "posaff.predicted"),
    must_have_rx = c("^b0c\\."),
    must_not     = c("\\.cooks$", "\\.leverage$", "^b0c\\.predicted$")
  )
)

# Compare one plot list against its EXPECT entry; print PASS/FAIL and details.
check_panels <- function(plots, key) {
  e  <- EXPECT[[key]]; nm <- names(plots)
  missing   <- setdiff(e$must_have, nm)
  rx_absent <- Filter(function(rx) !any(grepl(rx, nm)), e$must_have_rx)
  forbidden <- unique(unlist(lapply(e$must_not, function(rx) grep(rx, nm, value = TRUE))))
  render_err <- tryCatch({
    invisible(lapply(plots, function(p) ggplot2::ggplot_build(p))); character(0)
  }, error = function(err) conditionMessage(err))

  ok <- !length(missing) && !length(rx_absent) && !length(forbidden) && !length(render_err)
  cat(sprintf("  [%s] %s  (%d panels)\n", if (ok) "PASS" else "FAIL", key, length(nm)))
  if (length(missing))    cat("       missing   :", paste(missing, collapse = ", "), "\n")
  if (length(rx_absent))  cat("       no match  :", paste(rx_absent, collapse = ", "), "\n")
  if (length(forbidden))  cat("       forbidden :", paste(forbidden, collapse = ", "), "\n")
  if (length(render_err)) cat("       render err:", render_err, "\n")
  invisible(ok)
}

# Fit-and-check convenience: run both plot functions on a fitted model, check the
# residuals panels against `key`, and confirm distribution_plot renders.
check_model <- function(key, model) {
  rp <- residuals_plot(model)
  dp <- distribution_plot(model)
  ok_r <- check_panels(rp, key)
  ok_d <- tryCatch({ invisible(lapply(dp, ggplot2::ggplot_build)); TRUE },
                   error = function(e) { cat("       distribution_plot render err:",
                                             conditionMessage(e), "\n"); FALSE })
  invisible(ok_r && ok_d)
}
