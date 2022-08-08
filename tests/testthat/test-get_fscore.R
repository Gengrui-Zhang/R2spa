# TODO List:
# 1. Logical tests: e.g., lower bound of SE
# 2. Regression score and Bartlett scores


####################################### Test get_fscore function ######################################
# Loading packages and functions
library(lavaan)

########## Single-group example ##########

# Prepare test objects
# HL: The model should be a CFA model; may give a warning for non-CFA results
#     in future versions
myModel <- '
   # latent variables
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
'
model = myModel
# model = cfa_3var
data = PoliticalDemocracy
test_object_fs <- get_fs(PoliticalDemocracy, model)

########## Testing section ############

# Class of input

test_that("test the model input", {
  expect_type(myModel, "character")
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
                 cfa(model = myModel, data = PoliticalDemocracy),
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
  names_se <- grep("_se", fs_names, value = TRUE)
  for (j in names_se) {
      expect_identical(var(test_object_fs[[j]]), 0)
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


########################## Test fscore function ##############################

# Prepare for test objects
  fscore_model <- " ind60 =~ x1 + x2 + x3
                    dem60 =~ y1 + y2 + y3 + y4 "
  fit <- cfa(fscore_model, data = PoliticalDemocracy)
  fs_lavaan <- lavPredict(fit, method = "regression")
  est <- lavInspect(fit, what = "est")
  fscore_data <- lavInspect(fit, what = "data")
  test_object_fscore <- compute_fscore(fscore_data, lambda = est$lambda,
                                       theta = est$theta, psi = est$psi)

########## Testing section ############

  # Test the length of output is the same for fscore and lavaan function

  test_that("Test the length of output is equal", {
    expect_equal(nrow(test_object_fscore), nrow(fs_lavaan))
  })

  # Test the output is the same for fscore and lavaan function

  test_that("Test the output is the same for fscore and lavaan funtion", {
      expect_equal(test_object_fscore, fs_lavaan, ignore_attr = TRUE)
  })





