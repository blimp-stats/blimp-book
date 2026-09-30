# =============================================================================
# Regression tests for distribution_plot() and residuals_plot().
#
# These are SYNTHETIC: every model object comes from make_test_model() (see
# helper-make_test_model.R), so the suite runs under R CMD check with no Blimp
# and no data. Each test pins a decision from the Design Contract; a test that
# fails means a load-bearing decision was changed. Real-Blimp end-to-end
# coverage is a separate, gated integration script.
# =============================================================================

## small helpers -------------------------------------------------------------
# rows of the first GeomPoint layer (the bin/point cloud), sorted by x
.point_rows <- function(p) {
  b <- ggplot2::ggplot_build(p)
  g <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  d <- b$data[[which(g == "GeomPoint")[1]]]
  d <- d[, intersect(c("x", "y", "colour"), names(d)), drop = FALSE]
  d[order(d$x), , drop = FALSE]
}
.n_points <- function(p) nrow(.point_rows(p))
.builds   <- function(p) { ggplot2::ggplot_build(p); invisible(TRUE) }


test_that("residuals_plot dispatches the right panel set per outcome type", {
  rp <- residuals_plot(make_test_model("continuous"))
  expect_true(all(c("dpdd.predicted", "dpdd.index", "dpdd.leverage",
                    "dpdd.cooks") %in% names(rp)))            # linear-style suite

  rp <- residuals_plot(make_test_model("binary"))
  expect_true("drinker.1.binned" %in% names(rp))             # calibration
  expect_true(any(grepl("\\.probability$", names(rp))))      # predicted-prob overlay
  expect_false(any(grepl("\\.(cooks|leverage|index)$", names(rp))))  # none for nominal

  rp <- residuals_plot(make_test_model("count"))
  expect_true("events.index" %in% names(rp))                 # Pearson index
  expect_false("events.predicted" %in% names(rp))            # NO linear resid-vs-predicted
  expect_false(any(grepl("\\.(cooks|leverage)$", names(rp))))
})


test_that("Q-Q is suppressed for probabilities and categorical residuals, kept otherwise", {
  # categorical outcome residual + probability: histogram yes, Q-Q no
  for (tp in c("binary", "ordinal", "count", "multinomial")) {
    dp <- distribution_plot(make_test_model(tp))
    resid_qq <- grep("\\.residual\\.qq$", names(dp), value = TRUE)
    prob_qq  <- grep("\\.probability\\.qq$", names(dp), value = TRUE)
    expect_length(resid_qq, 0)     # categorical .residual Q-Q suppressed
    expect_length(prob_qq, 0)      # .probability Q-Q suppressed
  }
  # continuous residual, YJT, latent factor score, level-2 random intercept: Q-Q kept
  expect_true("dpdd.residual.qq"        %in% names(distribution_plot(make_test_model("continuous"))))
  expect_true("dpddcent.yjt.qq"         %in% names(distribution_plot(make_test_model("yjt"))))
  expect_true("inflamlat.latent.qq"     %in% names(distribution_plot(make_test_model("factor"))))
  expect_true("probsolve[school].qq"    %in% names(distribution_plot(make_test_model("mlm"))))
})


test_that("leverage/Cook's D appear only for single-level DVs", {
  expect_true(all(c("dpdd.leverage", "dpdd.cooks") %in%
                    names(residuals_plot(make_test_model("continuous")))))
  # clustered DV -> OLS influence suppressed
  expect_false(any(grepl("\\.(cooks|leverage)$",
                         names(residuals_plot(make_test_model("mlm"))))))
  expect_false(any(grepl("\\.(cooks|leverage)$",
                         names(residuals_plot(make_test_model("mlm_sem"))))))
})


test_that("within-cluster variance panel is continuous-outcome-only", {
  expect_true("probsolve.cluster_variance" %in%
                names(residuals_plot(make_test_model("mlm"))))       # continuous MLM DV
  expect_true("posaff.cluster_variance" %in%
                names(residuals_plot(make_test_model("mlm_sem"))))
})


test_that("MLM routes named latents and the random intercept to level-2 diagnostics", {
  rp <- residuals_plot(make_test_model("mlm_sem"))
  # b0c/b1c are level-2 latent outcomes: per-cluster index, NOT the level-1 suite
  expect_true("b0c.index" %in% names(rp))
  expect_true("b0c.pain.mean[person]" %in% names(rp))     # level-2 residual vs level-2 predictor
  expect_true("b1c.index" %in% names(rp))
  expect_false("b0c.predicted" %in% names(rp))            # not run through level-1 machinery

  rp <- residuals_plot(make_test_model("mlm"))
  expect_true("probsolve[school].index" %in% names(rp))   # bracketed random-intercept residual
})


test_that("point clouds stack all M imputations", {
  M <- 15
  rp <- residuals_plot(make_test_model("continuous", M = M, n = 300))
  # residual-vs-predictor cloud: ~ n * M points (stacked), not n (averaged)
  np <- .n_points(rp[["dpdd.inflam"]])
  expect_gt(np, 300 * M * 0.5)
})


test_that("binned residual points reproduce an independent recomputation exactly", {
  m <- make_test_model("binary", M = 8, n = 1200, seed = 5)
  imps <- lapply(m@imputations, as.data.frame)

  indep <- function(imps, xcol) {              # re-derive the documented algorithm
    xy <- lapply(imps, function(d) {
      xx <- if (xcol == "prob") d[["drinker.1.probability"]] else d[[xcol]]
      z <- data.frame(x = xx, y = (d$drinker == 1) - d[["drinker.1.probability"]])
      z[is.finite(z$x) & is.finite(z$y), ]
    })
    nkeep <- nrow(xy[[1]]); nb <- max(1L, floor(sqrt(nkeep)))
    allx <- unlist(lapply(xy, function(d) d$x))
    edges <- unique(stats::quantile(allx, seq(0, 1, length.out = nb + 1), na.rm = TRUE))
    per <- lapply(xy, function(d) {
      b <- cut(d$x, edges, include.lowest = TRUE, labels = FALSE)
      data.frame(bin = as.integer(names(tapply(d$x, b, mean))),
                 xm = as.numeric(tapply(d$x, b, mean)),
                 rm = as.numeric(tapply(d$y, b, mean)))
    })
    ab <- do.call(rbind, per)
    bs <- do.call(rbind, lapply(split(ab, ab$bin), function(z)
      data.frame(x = mean(z$xm), y = mean(z$rm))))
    bs[order(bs$x), ]
  }

  # calibration plot
  cal <- .point_rows(.binned_residuals(imps, "drinker", 1, .test_style()))
  ref <- indep(imps, "prob")
  expect_equal(max(abs(cal$x - ref$x), abs(cal$y - ref$y)), 0, tolerance = 1e-9)

  # vs-predictor plot
  vp  <- .point_rows(.binned_residuals_vs_predictor(imps, "drinker", 1, "agetryalc",
                                                    FALSE, .test_style()))
  ref <- indep(imps, "agetryalc")
  expect_equal(max(abs(vp$x - ref$x), abs(vp$y - ref$y)), 0, tolerance = 1e-9)
})


test_that("binned residuals are flat when correct and curved when misspecified", {
  set.seed(9); n <- 4000; x <- runif(n, -3, 3)

  # correct: p_hat = true probability -> unbiased, no trend
  p_true <- plogis(-0.4 + 1.1 * x); y <- rbinom(n, 1, p_true)
  ok <- replicate(6, data.frame(agetryalc = x, drinker = y,
                                "drinker.1.probability" = p_true, check.names = FALSE),
                  simplify = FALSE)
  b <- .point_rows(.binned_residuals_vs_predictor(ok, "drinker", 1, "agetryalc",
                                                  FALSE, .test_style()))
  expect_lt(abs(mean(b$y)), 0.015)                       # unbiased
  expect_lt(abs(coef(lm(y ~ x, data = b))[2]), 0.010)    # no trend

  # misspecified: true quadratic, fit linear -> inverted-U
  p2 <- plogis(-1 - 0.5 * x^2); y2 <- rbinom(n, 1, p2)
  ph <- predict(glm(y2 ~ x, family = binomial), type = "response")
  bad <- replicate(6, data.frame(agetryalc = x, drinker = y2,
                                 "drinker.1.probability" = ph, check.names = FALSE),
                   simplify = FALSE)
  b <- .point_rows(.binned_residuals_vs_predictor(bad, "drinker", 1, "agetryalc",
                                                  FALSE, .test_style()))
  expect_gt(mean(b$y[abs(b$x) < 1]), 0.03)               # under-predicts at the peak
  expect_lt(mean(b$y[abs(b$x) > 2]), -0.03)              # over-predicts at the tails
})


test_that("Cook's D standardizes by the single pooled sigma, not per-imputation s^2", {
  m <- make_test_model("continuous", M = 10, n = 250, seed = 3)
  imps <- lapply(m@imputations, as.data.frame)
  diag <- .residuals_lm_diagnostics(imps, "dpdd", c("inflam", "age"),
                                    character(0), FALSE)

  sig <- .pooled_sigma(imps, "dpdd")            # the shared benchmark
  p <- 3L; n <- nrow(imps[[1]]); M <- length(imps)
  ck <- matrix(NA_real_, n, M)
  for (t in seq_len(M)) {
    d <- imps[[t]]; X <- cbind(1, d$inflam, d$age); e <- d$dpdd.residual
    h <- diag(X %*% solve(crossprod(X)) %*% t(X))
    ck[, t] <- (e^2 / (p * sig^2)) * (h / (1 - h)^2)
  }
  expect_equal(diag$cooks, rowMeans(ck), tolerance = 1e-9)
})


test_that("group-mean-centered predictors are shown as within-cluster deviations", {
  rp <- residuals_plot(make_test_model("mlm"))
  # stanmath is on the groupmean= line -> its level-1 panel is the centered value
  p <- rp[["probsolve.stanmath"]]
  expect_true(!is.null(p))
  expect_match(p$labels$x, "group-mean centered")
})


test_that("every panel from every fixture actually renders", {
  for (tp in c("continuous","ordinal","binary","multinomial","count",
               "varmodel","yjt","factor","mlm","mlm_sem")) {
    m <- make_test_model(tp, M = 8)
    expect_silent(lapply(residuals_plot(m),  .builds))
    expect_silent(lapply(distribution_plot(m), .builds))
  }
})
