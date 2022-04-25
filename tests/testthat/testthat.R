library(testthat)
library(lavaan)
library(R2spa)

#TODO
#1. Check reliability score range [0,1]
#2. Check if number of rows is the same as original data

# Test get_fscore function
myModel <- '
   # latent variables
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
   # regressions
     dem60 ~ ind60
'
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

test_that("Test that the number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs), nrow(data))
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

test_that("Test the range of reliability scores", {
  Names <- colnames(test_object_fs)
  for (j in 1:length(Names[grepl("_se", Names)])) {
    for(i in 1:length(test_object_fs[Names[grepl("_se", Names)][j]])) {
      expect_gte(test_object_fs[Names[grepl("_se", Names)][j]][i,], 0)
      expect_lte(test_object_fs[Names[grepl("_se", Names)][j]][i,], 1)
    }
  }
})



# Test 2S-PA function

my_cfa <- '
   # latent variables
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
'

cfa_fit <- cfa(model = my_cfa,
               data  = PoliticalDemocracy)

fs_dat <- lavPredict(cfa_fit, se = "standard")
colnames(fs_dat) <- c("fs_ind60", "fs_dem60")
data <- fs_dat

tspa(my_cfa, fs_dat, reliability = c(ind60 = 0.9651282,
                                     dem60 = 0.9055203))

# Test function inputs

test_that ("Test the data length is larger than 1", {
  expect_gt(ncol(data), 1)
})

test_that ("Test the data variable names should contain prefix (fs_)", {
  Names <- colnames(data)
  expect_identical(grepl("fs_", Names), rep(TRUE, ncol(data)))
})

test_that ("Test the reliability length is larger than 1", {
  expect_gt(length(reliability), 1)
})

# Test function outputs

# Manually constructed 2spa model
my2spa <- '
   # latent variables (indicated by factor scores)
     ind60 =~ 1 * fs_ind60
     dem60 =~ 1 * fs_dem60
   # constrain the errors
     fs_ind60 ~~ ev1 * fs_ind60
     fs_dem60 ~~ ev2 * fs_dem60
   # latent variances
     ind60 ~~ v1 * ind60
     dem60 ~~ v2 * dem60
   # regressions
     dem60 ~ ind60
   # reliability constraints (reliability = v / (v + ev))
   # v = reliability / (1 - reliability) * ev
     v1 == 0.9651282 / (1 - 0.9651282) * ev1
     v2 == 0.9055203 / (1 - 0.9055203) * ev2
'

# Function generated model
var <- names(reliability)
len <- length(reliability)
group <- length(reliability[1])
col <- colnames(data)
fs <- paste0("fs_", var)

tspaModel <- rep(NA, len)
latent_var <- rep(NA, len)
error_constraint <- rep(NA, len)
latent_variance <- rep(NA, len)
reliability_constraint <- rep(NA, len)

for (x in 1:len) {
  latent_var[x] <- paste0(var[x], ' =~ 1 * ', fs[x], '\n')
  error_constraint[x] <- paste0(fs[x], ' ~~ ev', x, ' * ', fs[x], '\n')
  latent_variance[x] <- paste0(var[x], ' ~~ v', x, ' * ', var[x], '\n')
  reliability_constraint[x] <- paste0('v', x, ' == ', toString(reliability[x]), ' / ', toString(1 - reliability[x]), ' * ev', x, '\n')
  # latent variables fx =~ c(1, 1, 1, 1) * fs_fx
  latent_var[x] <- paste0(var[x], "=~ c(", paste0(rep(1, group), collapse = ", "), ") * ", fs[x], "\n")

  # constrain the errors fs_fx ~~ c(ev11, ev12, ev13, ev14) * fs_fx
  error_constraint[x] <- paste0(fs[x], "~~ c(", paste0(paste0(rep(paste0("ev",x), group), 1:group), collapse = ", "), ") * ", fs[x], "\n")

  # latent variances fx ~~ c(v11, v12, v13, v14) * fx
  latent_variance[x] <- paste0(var[x], " ~~ c(", paste0(paste0(rep(paste0("v",x), group), 1:group), collapse = ", "), ") * ", var[x], "\n")

  # reliability constraints v21 == (0.8854708 / (1 - 0.8854708)) * ev21 - b1^2 * v11
  if(x == 1) {
    reliability_constraint[x] <- paste0("v", x, 1:group, " == ", toString(reliability[x]), " / ", toString(1 - reliability[x]),
                                        " * ev", x, 1:group, "\n")
  }
  else {
    reliability_constraint[x] <- paste0("v", x, 1:group, " == ", toString(reliability[x]), " / ", toString(1 - reliability[x]),
                                        " * ev", x, 1:group, "- b", x - 1, "^2 * v", x - 1, 1:group, "\n")
  }
}

model <- paste0("fy ~ c(", paste0(rep("b1", group), collapse = ", "), ") * fx")
latent_var_str <- paste(latent_var, collapse="")
error_constraint_str <- paste(error_constraint, collapse="")
latent_variance_str <- paste(latent_variance, collapse="")
reliability_constraint_str <- paste(reliability_constraint, collapse="")
tspaModel <- paste0(latent_var_str, error_constraint_str, latent_variance_str, model, "\n", reliability_constraint_str)


test_that("Test the manually generated model is equal to function generated model", {
  expect_identical(my2spa, tspaModel)
})

test_that("Test the sem fit output is equal for two models", {
  expect_identical(tspa(model = my_cfa, data = fs_dat),
                   sem(model = tspaModel,data  = data))
})

# Test error and warning messages

test_that("Test the error message when the length of reliability is only 1", {
  expect_error(tspa(model, data, reliability = c(ind60 = 0.9651282)),
               "Reliability len is smaller than 2, unable to build model.
               Reliability needs to consist of at least 2 variables.")
})

test_that("Test the error message when data does not match the reliability", {
  expect_identical(paste0("fs_", names(reliability)), colnames(data))
})
