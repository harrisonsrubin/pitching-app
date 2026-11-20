# Run all tests from the repo root: Rscript tests/testthat.R
source("R/load.R")
library(testthat)
testthat::test_dir(file.path(repo_root(), "tests", "testthat"),
                   reporter = "summary", stop_on_failure = TRUE)
