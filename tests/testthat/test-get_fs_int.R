########################### Test get_fs_int function ##############################

# Load package and function
library(testthat)

# Simulate a dat
sample_dat <- data.frame(
  fs1 = rnorm(100),
  fs1_se = rnorm(100),
  fs1_ld = rnorm(100),
  fs2 = rnorm(100),
  fs2_se = rnorm(100),
  fs2_ld = rnorm(100),
  fs3 = rnorm(100),
  fs3_se = rnorm(100),
  fs3_ld = rnorm(100)
)

# Test that data input must be a data frame
test_that("Data input must be a data frame", {
  expect_error(get_fs_int(dat = list(),
                          fs_name = c("fs1", "fs2", "fs3"),
                          se_fs = c("fs1_se", "fs2_se", "fs3_se"),
                          loading_fs = c("fs1_ld", "fs2_ld", "fs3_ld")))
})

# Test for correct input checks
test_that("Inputs are checked for correct types and existence", {
  expect_error(get_fs_int(dat = sample_dat,
                          fs_name = c("fs1", "abc", "fs3"),
                          se_fs = c("fs1_se", "fs2_se", "fs3_se"),
                          loading_fs = c("fs1_ld", "fs2_ld", "fs3_ld")))
})

# Test for handling numeric lat_var and its length
test_that("lat_var must be numeric and match the length of fs_name", {
  expect_error(get_fs_int(dat = sample_dat,
                          fs_name = c("fs1", "fs2", "fs3"),
                          se_fs = c("fs1_se", "fs2_se", "fs3_se"),
                          loading_fs = c("fs1_ld", "fs2_ld", "fs3_ld"),
                          lat_var = "1"))
  expect_error(get_fs_int(dat = sample_dat,
                          fs_name = c("fs1", "fs2"),
                          se_fs = c("fs1_se", "fs2_se"),
                          loading_fs = c("fs1_ld", "fs2_ld"),
                          lat_var = c(1, 2, 3)))
})

# Test function with no `lat_var` or `model`
test_that("Product indicators are correctly calculated", {
  result <- get_fs_int(dat = sample_dat,
                       fs_name = c("fs1", "fs2", "fs3"),
                       se_fs = c("fs1_se", "fs2_se", "fs3_se"),
                       loading_fs = c("fs1_ld", "fs2_ld", "fs3_ld"))
  expect_true("fs1:fs2" %in% names(result))
  expect_true("fs1:fs2_se" %in% names(result))
  expect_true("fs1:fs2_ld" %in% names(result))
  expect_true("fs1:fs3" %in% names(result))
  expect_true("fs1:fs3_se" %in% names(result))
  expect_true("fs1:fs3_ld" %in% names(result))
  expect_true("fs2:fs3" %in% names(result))
  expect_true("fs2:fs3_se" %in% names(result))
  expect_true("fs2:fs3_ld" %in% names(result))
})

