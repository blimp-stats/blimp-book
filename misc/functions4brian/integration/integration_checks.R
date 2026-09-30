# =============================================================================
# integration_checks.R -- REAL-Blimp end-to-end check of the diagnostic plots.
#
# Unlike the synthetic tests/ suite (which needs no Blimp), this fits actual
# models and confirms the plot functions produce the right panels on genuine
# Blimp output -- i.e. it validates the Blimp interface (column naming, the
# @estimates dispersion key, the LATENT/clusterid parsing) that synthetic mocks
# can only approximate.
#
# It re-specifies only the "graphical diagnostics with multiple imputations"
# chunk (the nimps model) from each chapter script -- the other model runs in
# those scripts are for estimation/output and do not feed the plots. It reads
# the book's public data and does NOT modify any book script.
#
# Requirements: Blimp installed, rblimp, ggplot2, internet (data via GitHub).
# Run from this folder:   Rscript integration_checks.R
# Coverage here is the core + all-multilevel paths (the newest code). The Ch3
# single-level families (count, variance model, Yeo-Johnson, multinomial,
# factor) are pinned by the synthetic tests/ suite and exercised by the book's
# own run_all_examples.sh; add any of them below by pasting its nimps chunk and
# an EXPECT entry in expected_panels.R -- the pattern is identical.
# =============================================================================

suppressMessages({ library(ggplot2); library(rblimp) })
set_blimp("/applications/blimp/blimp-nightly")   # <-- EDIT to your Blimp path

source("../rblimp_cleaned_functions.R")          # the functions under test
source("expected_panels.R")                      # EXPECT + check_model()

gh <- function(name)
  read.csv(paste0("https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/", name))

cat("Real-Blimp integration checks\n=============================\n")

## ---- continuous single-level (Ch2 2.1) ----------------------------------
inflammation <- gh("inflammation.csv")
m_cont <- rblimp(
  data = inflammation, ordinal = "els female", center = "inflam age",
  model = "dpdd ~ inflam els female age",
  seed = 90291, burn = 10000, iter = 10000, nimps = 20)
check_model("continuous", m_cont)

## ---- binary nominal (Ch3 3.2) -------------------------------------------
alcohol <- gh("alcohol.csv")
m_bin <- rblimp(
  data = alcohol, ordinal = "college male", nominal = "drinker",
  center = "agetryalc age", model = "drinker ~ agetryalc college age male",
  seed = 90291, burn = 10000, iter = 10000, nimps = 20)
check_model("binary", m_bin)

## ---- MLM random intercept (Ch7 7.2) -------------------------------------
mathprobsolve <- gh("mathprobsolve.csv")
m_mlm <- rblimp(
  data = mathprobsolve, clusterid = "school", ordinal = "hispanic intschool",
  center = "groupmean = stanmath hispanic;
            grandmean = stanmath.mean hispanic.mean;",
  model = "probsolve ~ stanmath hispanic stanmath.mean hispanic.mean intschool | intercept;",
  seed = 90291, burn = 10000, iter = 10000, nimps = 20)
check_model("mlm", m_mlm)

## ---- MLM-SEM named latents (Ch7 7.6) ------------------------------------
diary <- gh("diary.csv")
m_sem <- rblimp(
  data = diary, clusterid = "person", latent = "person = b0c b1c",
  center = "groupmean = pain;
            grandmean = pain.mean;",
  model = "level2:
           b0c ~ intercept pain.mean;
           b1c ~ intercept;
           b0c ~~ b1c;
           level1:
           posaff ~ intercept@b0c pain@b1c;",
  seed = 90291, burn = 10000, iter = 10000, nimps = 20)
check_model("mlm_sem", m_sem)

cat("\nDone. Any FAIL above means the plot output diverged from the contract\n",
    "in expected_panels.R on real Blimp output -- investigate before shipping.\n", sep = "")
