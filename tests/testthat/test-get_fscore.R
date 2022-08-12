# TODO List:
# 1. Logical tests: e.g., lower bound of SE
# 2. Regression score and Bartlett scores

####################################### Test get_fs() function ######################################
# Loading packages and functions
library(lavaan)

########## Single-group example ##########

# Prepare test objects
# HL: The model should be a CFA model; may give a warning for non-CFA results
#     in future versions
# JZ: Thanks for correcting the model!
single_model <- '
                 # latent variables
                   ind60 =~ x1 + x2 + x3
                   dem60 =~ y1 + y2 + y3 + y4
                '

# model = cfa_3var
test_object_fs <- get_fs(PoliticalDemocracy, single_model)

########## Testing section ############

# Class of input

test_that("test the model input", {
  expect_type(single_model, "character")
})

test_that("test the data input", {
  expect_s3_class(PoliticalDemocracy, "data.frame")
})

# Class of output

test_that("Test that the function gives an output of data frame", {
  expect_s3_class(test_object_fs, "data.frame")
})

test_that("Test the number of factors is equal", {
  # HL: original test doesn't seem right
  expect_equal(length(test_object_fs) / 2,
               nrow(lavInspect(
                 cfa(model = single_model, data = PoliticalDemocracy),
                 what = "cor.lv"
               )))
})

test_that("Test that the number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs), nrow(PoliticalDemocracy))
})

# Test standard error

test_that("Test that standard errors for each observation are the same", {
  fs_names <- colnames(test_object_fs)  # HL: use small letter
  # HL: streamline the inner loop; use `seq_len(n)` instead of `1:n`
  # HL: use variables for things to be used many times
  # JZ: I like the idea of streamlining the inner loop. Thanks!
  names_se <- grep("_se", fs_names, value = TRUE)
  for (j in names_se) {
      expect_identical(var(test_object_fs[[j]]), 0)
  }
})

test_that("Test that standard errors for each observation are positive numbers and within 1", {
  fs_names <- colnames(test_object_fs)
  names_se <- grep("_se", fs_names, value = TRUE)
  for (j in names_se) {
    # HL: `expect_` functions do not return logical values
    expect_gt(min(test_object_fs[[j]]), 0)
    expect_lt(max(test_object_fs[[j]]), 1)
  }
})

# Bartlett scores
test_object_fs_bar <- get_fs(PoliticalDemocracy, single_model, method = "Bartlett")

test_that("Test that standard errors for each observation are the same", {
  fs_names <- colnames(test_object_fs_bar)
  names_se <- grep("_se", fs_names, value = TRUE)
  for (j in names_se) {
    expect_identical(var(test_object_fs_bar[[j]]), 0)
  }
})

test_that("Test that standard errors for each observation are positive numbers and within 1", {
  fs_names <- colnames(test_object_fs)
  names_se <- grep("_se", fs_names, value = TRUE)
  for (j in names_se) {
    expect_gt(min(test_object_fs_bar[[j]]), 0)
    expect_lt(max(test_object_fs_bar[[j]]), 1)
  }
})


########## multi-group examples ##########

###### One-factor example #####

# Prepare for test objects
hs_model <- 'visual  =~ x1 + x2 + x3'
# multi_fit <- cfa(hs_model,
#                  data = HolzingerSwineford1939,
#                  group = "school")
#get_fs(HolzingerSwineford1939, hs_model, group = "school")
test_object_fs_multi <-  get_fs(HolzingerSwineford1939[c("school", "x1", "x2", "x3")],
                                hs_model,
                                group = "school")

########## Testing section ############

# Class of output

test_that("Test the number of factors is equal", {
  # HL: Make it cleaner
  expect_equal(length(test_object_fs_multi),
               nrow(lavInspect(
                 cfa(model = hs_model, data = PoliticalDemocracy),
                 what = "cor.lv"
               )) * 2 + 1)
})

test_that("Test that the number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs_multi), nrow(HolzingerSwineford1939))
})

# Test standard errors

test_that("Test that standard errors for each observation are the same within groups", {
  test_se <- tapply(test_object_fs_multi$fs_visual_se,
                    test_object_fs_multi$school, var)
  for (i in length(test_se)) {
    expect_identical(unname(test_se[i]), 0)
  }
})

test_that("Test that standard errors for each observation are positive numbers and within 1", {
  # HL: Changed to not use & as I don't think the `expect_` functions return
  #     logical values
  expect_gt(min(test_object_fs_multi$fs_visual_se), 0)
  expect_lt(max(test_object_fs_multi$fs_visual_se), 1)
})

# Bartlett scores
test_object_fs_multi_bar <-  get_fs(HolzingerSwineford1939[c("school", "x1", "x2", "x3")],
                                    hs_model,
                                    group = "school",
                                    method = "Bartlett")

test_that("Test that standard errors for each observation are the same within groups", {
  test_se <- tapply(test_object_fs_multi_bar$fs_visual_se,
                    test_object_fs_multi_bar$school, var)
  for (i in length(test_se)) {
    expect_identical(unname(test_se[i]), 0)
  }
})

test_that("Test that standard errors for each observation are positive numbers and within 1", {
  expect_gt(min(test_object_fs_multi_bar$fs_visual_se), 0)
  expect_lt(max(test_object_fs_multi_bar$fs_visual_se), 1)
})

###### Multiple factors example #####

# Prepare for test objects
hs_model_2 <- ' visual =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed =~ x7 + x8 + x9 '
test_object_fs_multi_2 <- get_fs(HolzingerSwineford1939,
                                 hs_model_2,
                                 group = "school")

########## Testing section ############

# Class of output

test_that("Test the number of factors is equal", {
  expect_equal(length(test_object_fs_multi_2),
               nrow(lavInspect(
                 cfa(model = hs_model_2, data = HolzingerSwineford1939),
                 what = "cor.lv"
               )) * 2 + 1)
})

test_that("Test that the number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs_multi_2), nrow(HolzingerSwineford1939))
})

# Test standard errors

test_that("Test that standard errors for each observation are the same within groups", {
  # HL: redo the test
  fs_names <- colnames(test_object_fs_multi_2)  # HL: use small letter
  names_se <- grep("_se", fs_names, value = TRUE)
  for (i in names_se) {
    test_se <- tapply(test_object_fs_multi_2[[i]],
                      test_object_fs_multi$school, var)
    expect_identical(max(test_se), 0)
  }
})

test_that("Test that standard errors for each observation are positive numbers and within 1", {
  fs_names <- colnames(test_object_fs_multi_2)
  names_se <- grep("_se", fs_names, value = TRUE)
  for (i in names_se) {
      expect_gt(min(test_object_fs_multi_2[,i]), 0)
      expect_lt(max(test_object_fs_multi_2[,i]), 1)
  }
})

# Bartlett scores
test_object_fs_multi_2_bar <- get_fs(HolzingerSwineford1939,
                                     hs_model_2,
                                     group = "school")

test_that("Test that standard errors for each observation are the same within groups", {
  # HL: redo the test
  fs_names <- colnames(test_object_fs_multi_2_bar)  # HL: use small letter
  names_se <- grep("_se", fs_names, value = TRUE)
  for (i in names_se) {
    test_se <- tapply(test_object_fs_multi_2_bar[[i]],
                      test_object_fs_multi_2_bar$school, var)
    expect_identical(max(test_se), 0)
  }
})

test_that("Test that standard errors for each observation are positive numbers and within 1", {
  fs_names <- colnames(test_object_fs_multi_2_bar)
  names_se <- grep("_se", fs_names, value = TRUE)
  for (i in names_se) {
      expect_gt(min(test_object_fs_multi_2_bar[, i]), 0)
      expect_lt(max(test_object_fs_multi_2_bar[, i]), 1)
  }
})

