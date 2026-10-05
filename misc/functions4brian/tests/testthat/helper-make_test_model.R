# =============================================================================
# make_test_model() -- synthetic rblimp-style model objects for testing the
# diagnostic plot functions WITHOUT Blimp or any data set.
#
# It fabricates the exact @slots and column-naming that distribution_plot() and
# residuals_plot() read, one layout per model branch. This doubles as the
# machine-readable spec for the model-object interface: whatever slots/columns
# this helper populates are precisely what the plot code consumes.
#
# Slots consumed by the plot code (this is the whole interface):
#   @imputations   list of M data frames (one per imputation), identical columns
#   @average_imp   one data frame: per-case mean across imputations (modal for
#                  categoricals). Used for DV detection and predictor lookup.
#   @variance_imp  named list/data frame flagging iteration-varying cases (>0);
#                  distribution_plot only. NULL is acceptable for residuals_plot.
#   @syntax        list with $model and, when present, $clusterid, $latent,
#                  $center, $ordinal, $nominal, $count (each a character string).
#   @estimates     matrix with rownames; only the count path reads it, at
#                  row "<dv> Variance Dispersion Parameter", column "Estimate".
#
# Column-naming grammar the layouts follow (see the file header's naming block):
#   LEVEL-1 (per row):  <dv>.residual  <dv>.predicted  <dv>.probability
#                       <dv>.<k>.residual / <dv>.<k>.probability  (nominal cat k)
#                       <var>.latent  <var>.yjt  var.<dv>.predicted
#   LEVEL-2 (per cluster, bracketed):  <dv>[cluster]  <x>.mean[cluster]
# =============================================================================

# Minimal S4 stand-in for an rblimp model object. In the package the real class
# is used; here we only need the slots the plot code reads.
methods::setClass(
  "blimp_mock",
  representation(imputations = "list", average_imp = "ANY",
                variance_imp = "ANY", syntax = "ANY", estimates = "ANY")
)

# Assemble a mock from a per-imputation data-frame builder `mk(i)`.
.assemble_mock <- function(mk, M, syntax, estimates = NULL) {
  imps <- lapply(seq_len(M), function(i) mk(i))
  # average_imp: per-case mean of numeric columns, first value for the rest.
  avg <- imps[[1]]
  for (nm in names(avg)) {
    if (is.numeric(avg[[nm]])) {
      avg[[nm]] <- Reduce(`+`, lapply(imps, function(d) d[[nm]])) / M
    }
  }
  # variance_imp: Blimp flags iteration-varying cases with nonzero entries.
  # Model-implied and level-2 columns vary every iteration (all 1); raw observed
  # columns are treated as fully observed here (all 0). distribution_plot reads
  # this to split observed vs imputed/model-implied values.
  implied_rx <- "(\\.(residual|predicted|probability|latent|yjt)$)|(^var\\.)|(\\[.+\\]$)"
  vimp <- imps[[1]]
  for (nm in names(vimp)) {
    vimp[[nm]] <- if (grepl(implied_rx, nm)) rep(1, nrow(vimp)) else rep(0, nrow(vimp))
  }
  methods::new("blimp_mock", imputations = imps, average_imp = avg,
               variance_imp = vimp, syntax = syntax, estimates = estimates)
}

# Build an @estimates matrix whose ROWNAMES enumerate each DV's fixed-effects
# design, mirroring Blimp's coefficient table ("<dv> ~ Intercept", "<dv> ~ <term>").
# residuals_plot reads only these rownames to source the leverage/Cook's design,
# so the Estimate values are placeholders. `design` is a named list dv -> terms;
# `extra` adds any non-design rows (e.g. the count dispersion parameter).
.mk_estimates <- function(design, extra = NULL) {
  rows <- unlist(lapply(names(design), function(dv)
    c(paste0(dv, " ~ Intercept"), paste0(dv, " ~ ", design[[dv]]))),
    use.names = FALSE)
  rows <- c(rows, names(extra))
  m <- matrix(c(rep(0, length(rows) - length(extra)), as.numeric(extra)),
              ncol = 1, dimnames = list(rows, "Estimate"))
  m
}

#' Build a synthetic rblimp-style model for one diagnostic branch.
#'
#' @param type one of: "continuous", "ordinal", "binary", "multinomial",
#'   "count", "varmodel", "yjt", "factor", "mlm", "mlm_sem".
#' @param M number of imputations. @param n level-1 rows. @param J clusters.
make_test_model <- function(type = "continuous", M = 20, n = 400, J = 40,
                            seed = 1) {
  set.seed(seed)
  type <- match.arg(type, c("continuous", "ordinal", "binary", "multinomial",
                            "count", "varmodel", "yjt", "factor", "mlm",
                            "mlm_sem"))

  switch(type,

    ## ---- continuous single-level (2.1): ordinal predictors present ----------
    continuous = {
      x  <- rnorm(n); z <- rnorm(n)
      els <- rbinom(n, 1, .5); female <- rbinom(n, 1, .5)
      mk <- function(i) data.frame(
        dpdd = x + z + rnorm(n), inflam = x, age = z,
        els = els, female = female,
        dpdd.residual = rnorm(n), dpdd.predicted = x + z,
        check.names = FALSE)
      .assemble_mock(mk, M, list(model = "dpdd ~ inflam els female age",
                                 ordinal = "els female", center = "inflam age"),
                     estimates = .mk_estimates(list(
                       dpdd = c("inflam", "els", "female", "age"))))
    },

    ## ---- ordinal OUTCOME (synthetic; no dedicated book script) --------------
    # Ordinal DVs pair <dv>.residual with <dv>.predicted (linear-style suite)
    # and expose cumulative <dv>.<k>.probability columns for the overlay.
    ordinal = {
      x <- rnorm(n)
      mk <- function(i) data.frame(
        ordsev = sample(1:4, n, TRUE), x = x,
        ordsev.residual = rnorm(n), ordsev.predicted = x,
        "ordsev.1.probability" = plogis(rnorm(n)),
        "ordsev.2.probability" = plogis(rnorm(n)),
        "ordsev.3.probability" = plogis(rnorm(n)),
        check.names = FALSE)
      .assemble_mock(mk, M, list(model = "ordsev ~ x", ordinal = "ordsev"),
                     estimates = .mk_estimates(list(ordsev = "x")))
    },

    ## ---- binary nominal outcome (3.2) ---------------------------------------
    binary = {
      x <- rnorm(n); male <- rbinom(n, 1, .5); college <- rbinom(n, 1, .5)
      y <- rbinom(n, 1, plogis(-0.3 + 0.7 * x))
      mk <- function(i) data.frame(
        drinker = y, agetryalc = x, age = rnorm(n),
        male = male, college = college,
        "drinker.1.residual" = (y == 1) - plogis(-0.3 + 0.7 * x),
        "drinker.1.probability" = plogis(-0.3 + 0.7 * x + rnorm(n, 0, .05)),
        check.names = FALSE)
      .assemble_mock(mk, M, list(model = "drinker ~ agetryalc college age male",
                                 nominal = "drinker", ordinal = "college male",
                                 center = "agetryalc age"))
    },

    ## ---- multinomial nominal outcome, K>1 (3.3) -----------------------------
    multinomial = {
      x <- rnorm(n); y <- sample(0:2, n, TRUE)
      mk <- function(i) data.frame(
        drinkfreq = y, agetryalc = x, age = rnorm(n),
        "drinkfreq.1.residual" = (y == 1) - 0.33,
        "drinkfreq.1.probability" = plogis(rnorm(n)),
        "drinkfreq.2.residual" = (y == 2) - 0.33,
        "drinkfreq.2.probability" = plogis(rnorm(n)),
        check.names = FALSE)
      .assemble_mock(mk, M, list(model = "drinkfreq ~ agetryalc age",
                                 nominal = "drinkfreq"))
    },

    ## ---- count / negative binomial (3.4): reads @estimates dispersion key ---
    count = {
      x <- rnorm(n); y <- rpois(n, exp(0.5 + 0.3 * x))
      mk <- function(i) data.frame(
        events = y, x = x,
        events.residual = rnorm(n), events.predicted = exp(0.5 + 0.3 * x),
        check.names = FALSE)
      est <- matrix(0.8, 1, 1,
                    dimnames = list("events Variance Dispersion Parameter",
                                    "Estimate"))
      .assemble_mock(mk, M, list(model = "events ~ x", count = "events"),
                     estimates = est)
    },

    ## ---- variance model (3.5): var.<dv>.predicted -> .pooled_sigma per-obs --
    varmodel = {
      x <- rnorm(n)
      mk <- function(i) data.frame(
        dpdd = x + rnorm(n), inflam = x,
        dpdd.residual = rnorm(n) * exp(0.2 * x), dpdd.predicted = x,
        "var.dpdd.predicted" = 0.4 * x,   # per-case log-variance
        check.names = FALSE)
      .assemble_mock(mk, M, list(model = "dpdd ~ inflam", ordinal = ""),
                     estimates = .mk_estimates(list(dpdd = "inflam")))
    },

    ## ---- Yeo-Johnson (3.6): yjt() DV reduces to <base>.residual/.predicted --
    yjt = {
      x <- rnorm(n)
      mk <- function(i) data.frame(
        dpddcent = x + rnorm(n), inflam = x,
        dpddcent.residual = rnorm(n), dpddcent.predicted = x,
        "dpddcent.yjt" = x + rnorm(n),
        check.names = FALSE)
      .assemble_mock(mk, M, list(model = "yjt(dpddcent) ~ inflam",
                                 ordinal = ""),
                     estimates = .mk_estimates(list(dpddcent = "inflam")))
    },

    ## ---- factor / measurement model (Ch6): f.latent + `->` loadings ---------
    # Indicators are the DVs; the factor score enters as a `<f>.latent` predictor.
    factor = {
      f <- rnorm(n)
      mk <- function(i) data.frame(
        x1 = f + rnorm(n), x2 = f + rnorm(n), x3 = f + rnorm(n),
        "inflamlat.latent" = f,
        x1.residual = rnorm(n), x1.predicted = f,
        x2.residual = rnorm(n), x2.predicted = f,
        x3.residual = rnorm(n), x3.predicted = f,
        check.names = FALSE)
      .assemble_mock(mk, M, list(model = "inflamlat -> x1 x2 x3"),
                     estimates = .mk_estimates(list(x1 = "inflamlat",
                                                    x2 = "inflamlat",
                                                    x3 = "inflamlat")))
    },

    ## ---- MLM random intercept (7.2): probsolve[school] ----------------------
    mlm = {
      ni <- n %/% J; n2 <- ni * J; cl <- rep(seq_len(J), each = ni)
      u0 <- rnorm(J); sm <- rnorm(J); hm <- rnorm(J)
      mk <- function(i) data.frame(
        school = cl, probsolve = rnorm(n2),
        stanmath = rnorm(n2), hispanic = rbinom(n2, 1, .5),
        intschool = rbinom(n2, 1, .5),
        "stanmath.mean[school]" = sm[cl], "hispanic.mean[school]" = hm[cl],
        "probsolve[school]" = u0[cl],
        probsolve.residual = rnorm(n2), probsolve.predicted = rnorm(n2),
        check.names = FALSE)
      .assemble_mock(mk, M, list(
        clusterid = "school", ordinal = "hispanic intschool",
        center = "groupmean = stanmath hispanic; grandmean = stanmath.mean hispanic.mean;",
        model = "probsolve ~ stanmath hispanic stanmath.mean hispanic.mean intschool"))
    },

    ## ---- MLM-SEM named latents (7.6): b0c/b1c -------------------------------
    mlm_sem = {
      ni <- n %/% J; n2 <- ni * J; cl <- rep(seq_len(J), each = ni)
      b0 <- rnorm(J); b1 <- rnorm(J); pm <- rnorm(J)
      mk <- function(i) data.frame(
        person = cl, posaff = rnorm(n2), pain = rnorm(n2),
        "pain.mean[person]" = pm[cl],
        "b0c.latent" = b0[cl], "b1c.latent" = b1[cl],
        b0c.residual = b0[cl], b1c.residual = b1[cl],
        posaff.residual = rnorm(n2),
        b0c.predicted = 0.3 * pm[cl], b1c.predicted = 0 * b1[cl],
        posaff.predicted = rnorm(n2),
        check.names = FALSE)
      .assemble_mock(mk, M, list(
        clusterid = "person", latent = "person = b0c b1c",
        center = "groupmean = pain; grandmean = pain.mean;",
        model = "b0c ~ intercept pain.mean; b1c ~ intercept; b0c ~~ b1c; posaff ~ intercept@b0c pain@b1c;"))
    }
  )
}


# Default residuals_plot style bundle, for calling internal plot helpers directly.
.test_style <- function() list(
  font_size = 14, point_color = "#00B0F6", curve_color = "#F8766D",
  point_alpha = 0.70, point_size = 1.5, line_width = 1.0,
  max_labels = 10, label_family = "", few_values_warn = 5
)
