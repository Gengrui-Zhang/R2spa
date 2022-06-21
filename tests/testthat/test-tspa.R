########################################## Test 2S-PA function ##########################################
# Loading packages and functions
library(lavaan)
devtools::load_all()

########## Single-group example ##########

# Prepare test objects

# Example 1: Single-group with two variables
    cfa_ex1 <- '# latent variables
                ind60 =~ x1 + x2 + x3
                dem60 =~ y1 + y2 + y3 + y4
                '
    cfa_fit_ex1 <- cfa(model = my_cfa,
                   data  = PoliticalDemocracy)

    # get factor scores
    fs_dat_ex1 <- get_fs(PoliticalDemocracy, my_cfa)

    # tspa model
    tspa_ex1 <- tspa(model = "dem60 ~ ind60", data = fs_dat_ex1,
              se = data.frame(ind60 = 0.1273703, dem60 = 0.6761707))
    summary(tspa_ex1)

########## Testing section ############

# Class of input
    var_len <- 2

    # The tspa data should be composed of three parts: variable, se, reliability
    test_that("test the number of columns in tspa data are multiples of the variable length", {
      expect_equal(var_len*3, ncol(fs_dat_ex1))
    })

    test_that("test if the length of se is the same as the length of variable", {
      expect_equal(var_len, length(se))
    })

# Class of output





# Example 2:

# cfa model
cfa_3var <- '
# latent variables
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8

# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'

# get factor scores
fs_3var_dat <- get_fs(PoliticalDemocracy, cfa_3var)

# tspa model
tspa(model = "dem60 ~ ind60
              dem65 ~ ind60 + dem60",
     data = fs_3var_dat,
     se = data.frame(ind60 = 0.1267792, dem60 = 0.6863648, dem65 = 0.6074362))



# my_cfa <- '
#    # latent variables
#      ind60 =~ x1 + x2 + x3
#      dem60 =~ y1 + y2 + y3 + y4
# '
#
# cfa_fit <- cfa(model = my_cfa,
#                data  = PoliticalDemocracy)
#
# fs_dat <- lavPredict(cfa_fit, se = "standard")
# colnames(fs_dat) <- c("fs_ind60", "fs_dem60")
# data <- fs_dat
#
# reliability = c(ind60 = 0.9651282,
#                 dem60 = 0.9055203)
#
# tspa(my_cfa, fs_dat, reliability = c(ind60 = 0.9651282,
#                                      dem60 = 0.9055203))

########## Testing section #############

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
