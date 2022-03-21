library(testthat)
library(R2spa)

# Test get_fscore function
model = myModel
model = cfa_3var
data = PoliticalDemocracy
test_object_fs <- get_fs(PoliticalDemocracy, model)
fit <- sqrt(length(unlist(lavInspect(cfa(model, data = data))["psi"])))

# Class of input

test_that("test the model input", {
  expect_type(model, "character")
})

test_that("test the data input", {
  expect_s3_class(data, "data.frame")
})

# Class of output

test_that("Test that the function gives an output of data frame", {
  expect_s3_class(test_object_fs, "data.frame")
})

test_that("Test the length of output", {
  expect_equal(length(test_object_fs)/2, sqrt(length(unlist(lavInspect(cfa(model, data = data))["psi"]))))
})

# Test reliability scores

test_that("Test that reliability scores for each observation are the same", {
  Names <- colnames(test_object_fs)
  for (j in 1:length(Names[grepl("_se", Names)])) {
    for(i in 1:length(test_object_fs[Names[grepl("_se", Names)][j]])) {
      expect_identical(test_object_fs[Names[grepl("_se", Names)][j]][i], test_object_fs[Names[grepl("_se", Names)][j]][1])
    }
  }
})

# Test 2S-PA function

# It seems that there is an error of parsing the error constraints?

tspa(model = "dem60 ~ ind60
              dem65 ~ ind60 + dem60",
     data = fs_3var_dat, reliability = c(ind60 = 0.9968282, dem60 = 0.8503460, dem65 = 0.8503460))

tspa(model = "dem60 ~ ind60", data = fs_dat,
     reliability = c(ind60 = 0.9651282, dem60 = 0.9055203))


