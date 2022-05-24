# TODO
# Use different examples to test (e.g., more factors/more groups)

####################################### Test get_fscore function ######################################
# Loading packages and functions
library(testthat)
library(lavaan)
source("/Users/jimmy_z/R Projects/R2spa/R/get_fscore.R")

########## Single-group example ##########

# Prepare for test objects
myModel <- '
   # latent variables
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
   # regressions
     dem60 ~ ind60
'
model = myModel
# model = cfa_3var
data = PoliticalDemocracy
test_object_fs <- get_fs(PoliticalDemocracy, model)
fit <- sqrt(length(unlist(lavInspect(cfa(myModel, PoliticalDemocracy))["psi"])))

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
  expect_equal(length(test_object_fs), sqrt(length(unlist(lavInspect(cfa(model = myModel, data = PoliticalDemocracy))["psi"])))*3)
})

test_that("Test that the number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs), nrow(PoliticalDemocracy))
})

# Test reliability scores

test_that("Test that reliability scores for each observation are the same", {
  Names <- colnames(test_object_fs)
  for (j in 1:length(Names[grepl("_rel", Names)])) {
    for(i in 1:length(test_object_fs[Names[grepl("_rel", Names)][j]])) {
      expect_identical(test_object_fs[Names[grepl("_rel", Names)][j]][i], test_object_fs[Names[grepl("_rel", Names)][j]][1])
    }
  }
})

test_that("Test the range of reliability scores", {
  Names <- colnames(test_object_fs)
  for (j in 1:length(Names[grepl("_rel", Names)])) {
    for(i in 1:length(test_object_fs[Names[grepl("_rel", Names)][j]])) {
      expect_gte(test_object_fs[Names[grepl("_rel", Names)][j]][i,], 0)
      expect_lte(test_object_fs[Names[grepl("_rel", Names)][j]][i,], 1)
    }
  }
})

# Test standard error

test_that("Test that standard errors for each observation are the same", {
  Names <- colnames(test_object_fs)
  for (j in 1:length(Names[grepl("_se", Names)])) {
    for(i in 1:length(test_object_fs[Names[grepl("_se", Names)][j]])) {
      expect_identical(test_object_fs[Names[grepl("_se", Names)][j]][i], test_object_fs[Names[grepl("_se", Names)][j]][1])
    }
  }
})

########## multi-group examples ##########

###### One-factor example #####

# Prepare for test objects
    hs_model <- 'visual  =~ x1 + x2 + x3'
    # multi_fit <- cfa(hs_model,
    #                  data = HolzingerSwineford1939,
    #                  group = "school")
    get_fs(HolzingerSwineford1939, hs_model, group = "school")
    test_object_fs_multi <-  get_fs(HolzingerSwineford1939[c("school", "x4", "x5", "x6")],
                                    group = "school")

########## Testing section ############

# Class of output

test_that("Test the number of factors is equal", {
  expect_equal(length(test_object_fs_multi),
               (sqrt(length(unlist(lavInspect(cfa(model = hs_model, data = HolzingerSwineford1939))["psi"])))*3 + 1))
})

test_that("Test that the number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs_multi), nrow(HolzingerSwineford1939))
})

# Test standard errors

test_that("Test that standard errors for each observation are the same within groups", {
  test_se <- tapply(test_object_fs_multi$fs_f1_se, test_object_fs_multi$school, var)
  for (i in length(test_se)) {
    expect_identical(unname(test_se[i]), 0)
  }
})

# Test reliability scores

test_that("Test that reliability scores for each observation are the same within groups", {
  test_rel <- tapply(test_object_fs_multi$fs_f1_rel, test_object_fs_multi$school, var)
  for (i in length(test_rel)) {
    expect_identical(unname(test_rel[i]), 0)
  }
})

###### Multiple factors example #####

# Prepare for test objects
  hs_model_2 <- ' visual =~ x1 + x2 + x3
                          textual =~ x4 + x5 + x6
                          speed =~ x7 + x8 + x9 '
  test_object_fs_multi_2 <- get_fs(HolzingerSwineford1939, hs_model_2, group = "school")

########## Testing section ############

# Class of output

test_that("Test the number of factors is equal", {
  expect_equal((length(test_object_fs_multi_2) - 1)/3,
               (sqrt(length(unlist(lavInspect(cfa(model = hs_model_2, data = HolzingerSwineford1939))["psi"])))))
})

test_that("Test that the number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs_multi_2), nrow(HolzingerSwineford1939))
})

# Test standard errors

test_that("Test that standard errors for each observation are the same within groups", {
  Names <- colnames(test_object_fs_multi_2)
  factors <- Names[grepl("fs_", Names) & !grepl("_se", Names) & !grepl("_rel", Names)]
  for (i in 1:length(factors)) {
    test_se <- tapply(test_object_fs_multi_2[, paste0(factors[i], "_se")], test_object_fs_multi$school, var)
    for (j in length(test_se)) {
      expect_identical(unname(test_se[j]), 0)
    }
  }
})

# Test reliability scores

test_that("Test that reliability scores for each observation are the same within groups", {
  Names <- colnames(test_object_fs_multi_2)
  factors <- Names[grepl("fs_", Names) & !grepl("_se", Names) & !grepl("_rel", Names)]
  for (i in 1:length(factors)) {
    test_rel <- tapply(test_object_fs_multi_2[, paste0(factors[i], "_rel")], test_object_fs_multi$school, var)
    for (j in length(test_rel)) {
      expect_identical(unname(test_rel[j]), 0)
    }
  }
})


####################################### Test fscore function ######################################

# Prepare for test objects
  fscore_model <- " ind60 =~ x1 + x2 + x3
                    dem60 =~ y1 + y2 + y3 + y4 "
  fit <- cfa(fscore_model, data = PoliticalDemocracy)
  fs_lavaan <- lavPredict(fit, method = "regression")
  est <- lavInspect(fit, what = "est")
  fscore_data <- lavInspect(fit, what = "data")
  test_object_fscore <- fscore(fscore_data, lambda = est$lambda, theta = est$theta, psi = est$psi)

########## Testing section ############

  # Test the length of output is the same for fscore and lavaan function

  test_that("Test the length of output is equal", {
    expect_equal(nrow(test_object_fscore), nrow(fs_lavaan))
  })

  # Test the output is the same for fscore and lavaan function

  test_that("Test the output is the same for fscore and lavaan funtion", {
      for (i in 1:length(as.vector(test_object_fscore))) {
        expect_equal((round(as.vector(test_object_fscore)[i],5) - round(as.vector(fs_lavaan)[i],5)), 0)
      }
    }
  )





