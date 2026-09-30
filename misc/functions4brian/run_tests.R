# =============================================================================
# run_tests.R -- run the synthetic regression suite standalone (no Blimp, no
# data). From this folder:   Rscript run_tests.R
# In a package, testthat runs tests/testthat/ automatically and this file is
# unnecessary; it exists so the suite can be verified before packaging.
# =============================================================================
suppressMessages({library(ggplot2); library(testthat)})
source("rblimp_cleaned_functions.R")                 # load the functions under test
testthat::test_dir("tests/testthat", reporter = "summary")  # auto-loads helper-*.R
