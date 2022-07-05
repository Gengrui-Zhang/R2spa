###Comments-0621###
1. Get factors scores separately using Separate One-Factor CFAs (error vs reliability.Rmd)
2. Test the difference betwee two parameters: choose a threshold, and test the max value of the absolute differences
  e.g., max(abs(par1 - par2)) < 0.1


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
                # regression
                dem60 ~ ind60
                '
    cfa_fit_ex1 <- sem(model = cfa_ex1,
                   data  = PoliticalDemocracy)
    cfa_output_ex1 <- summary(cfa_fit_ex1)

    # get factor scores
    fs_dat_ex1 <- get_fs(PoliticalDemocracy, cfa_ex1)

    # tspa model
    tspa_ex1 <- tspa(model = "dem60 ~ ind60", data = fs_dat_ex1,
              se = data.frame(ind60 = 0.1273703, dem60 = 0.6761707))
    tspa_output_ex1 <- summary(tspa_ex1)

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

    test_that ("Test the data length is larger than 1", {
      expect_gt(ncol(fs_dat_ex1), 1)
    })

    test_that ("Test the data variable names should contain prefix (fs_)", {
      Names <- colnames(fs_dat_ex1)
      expect_identical(grepl("fs_", Names), rep(TRUE, ncol(fs_dat_ex1)))
    })

# Class of output

    # Parameter estimates

    cfa_output_ex1 <- as.data.frame(cfa_output_ex1$PE)
    tspa_output_ex1 <- as.data.frame(tspa_output_ex1$PE)

    test_that("test if the regression coefficient of factors is the same for two methods", {
      expect_equal(cfa_output_ex1[cfa_output_ex1$op == "~", ]$est, tspa_output_ex1[tspa_output_ex1$op == "~", ]$est)
    })

    test_that("test if the se of regression coefficient is the same for two methods", {
      expect_equal(cfa_output_ex1[cfa_output_ex1$op == "~", ]$se, tspa_output_ex1[tspa_output_ex1$op == "~", ]$se)
    })

    # Variances of factors

    test_that("test if the variance of factor is the same for two methods", {
      expect_equal(cfa_output_ex1[cfa_output_ex1$lhs == "ind60" & cfa_output_ex1$rhs == "ind60" & cfa_output_ex1$op == "~~", ]$est,
                   tspa_output_ex1[tspa_output_ex1$lhs == "ind60" & tspa_output_ex1$rhs == "ind60" & tspa_output_ex1$op == "~~", ]$est)
    })

    test_that("test if the se of variance is the same for two methods", {
      expect_equal(cfa_output_ex1[cfa_output_ex1$lhs == "ind60" & cfa_output_ex1$rhs == "ind60" & cfa_output_ex1$op == "~~", ]$se,
                   tspa_output_ex1[tspa_output_ex1$lhs == "ind60" & tspa_output_ex1$rhs == "ind60" & tspa_output_ex1$op == "~~", ]$se)
    })

    test_that("test if the variance of factor is the same for two methods", {
      expect_equal(cfa_output_ex1[cfa_output_ex1$lhs == "dem60" & cfa_output_ex1$rhs == "dem60" & cfa_output_ex1$op == "~~", ]$est,
                   tspa_output_ex1[tspa_output_ex1$lhs == "dem60" & tspa_output_ex1$rhs == "dem60" & tspa_output_ex1$op == "~~", ]$est)
    })

    test_that("test if the se of variance is the same for two methods", {
      expect_equal(cfa_output_ex1[cfa_output_ex1$lhs == "dem60" & cfa_output_ex1$rhs == "dem60" & cfa_output_ex1$op == "~~", ]$se,
                   tspa_output_ex1[tspa_output_ex1$lhs == "dem60" & tspa_output_ex1$rhs == "dem60" & tspa_output_ex1$op == "~~", ]$se)
    })

    # Fit measures

    cfa_fitmeasures_ex1 <- as.data.frame(fitmeasures(cfa_fit_ex1))
    tspa_fitmeasures_ex1 <- as.data.frame(fitmeasures(tspa_ex1))

    test_that("test if chisq is the same for two methods", {
      expect_equal(cfa_fitmeasures_ex1["chisq", ], tspa_fitmeasures_ex1["chisq", ])
      # We can add more comparisons of fitting measures by changing the name
    })

    test_that("test if chisq is the same for two methods", {
      expect_equal(cfa_fitmeasures_ex1["rmsea", ], tspa_fitmeasures_ex1["rmsea", ])
      # We can add more comparisons of fitting measures by changing the name
    })

# Example 2: Single group with three variables

    cfa_ex2 <- '
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
                   # Regression
                     dem60 ~ ind60
                     dem65 ~ ind60 + dem60
                '
    cfa_fit_ex2 <- sem(model = cfa_ex2,
                        data  = PoliticalDemocracy)
    cfa_output_ex2 <- summary(cfa_fit_ex2)

    # get factor scores
    fs_dat_ex2 <- get_fs(PoliticalDemocracy, cfa_ex2)

    # tspa model
    tspa_ex2 <- tspa(model = "dem60 ~ ind60
                              dem65 ~ ind60 + dem60",
                     data = fs_dat_ex2,
                     se = data.frame(ind60 = 0.1267792, dem60 = 0.6863648, dem65 = 0.6074362))
    tspa_output_ex2 <- summary(tspa_ex2)

########## Testing section #############

    # Parameter estimates

    cfa_output_ex2 <- as.data.frame(cfa_output_ex2$PE)
    tspa_output_ex2 <- as.data.frame(tspa_output_ex2$PE)

    test_that("test if the regression coefficient of factors is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "dem60" & cfa_output_ex2$rhs == "ind60" & cfa_output_ex2$op == "~", ]$est,
                   tspa_output_ex2[tspa_output_ex2$lhs == "dem60" & tspa_output_ex2$rhs == "ind60" & tspa_output_ex2$op == "~", ]$est)
    })

    test_that("test if the se of regression coefficient is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "dem60" & cfa_output_ex2$rhs == "ind60" & cfa_output_ex2$op == "~", ]$se,
                   tspa_output_ex2[tspa_output_ex2$lhs == "dem60" & tspa_output_ex2$rhs == "ind60" & tspa_output_ex2$op == "~", ]$se)
    })

    test_that("test if the regression coefficient of factors is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "dem65" & cfa_output_ex2$rhs == "ind60" & cfa_output_ex2$op == "~", ]$est,
                   tspa_output_ex2[tspa_output_ex2$lhs == "dem65" & tspa_output_ex2$rhs == "ind60" & tspa_output_ex2$op == "~", ]$est)
    })

    test_that("test if the se of regression coefficient is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "dem65" & cfa_output_ex2$rhs == "ind60" & cfa_output_ex2$op == "~", ]$se,
                   tspa_output_ex2[tspa_output_ex2$lhs == "dem65" & tspa_output_ex2$rhs == "ind60" & tspa_output_ex2$op == "~", ]$se)
    })

    test_that("test if the regression coefficient of factors is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "dem65" & cfa_output_ex2$rhs == "dem60" & cfa_output_ex2$op == "~", ]$est,
                   tspa_output_ex2[tspa_output_ex2$lhs == "dem65" & tspa_output_ex2$rhs == "dem60" & tspa_output_ex2$op == "~", ]$est)
    })

    test_that("test if the se of regression coefficient is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "dem65" & cfa_output_ex2$rhs == "dem60" & cfa_output_ex2$op == "~", ]$se,
                   tspa_output_ex2[tspa_output_ex2$lhs == "dem65" & tspa_output_ex2$rhs == "dem60" & tspa_output_ex2$op == "~", ]$se)
    })

    # Variance of factors

    test_that("test if the variance of factor is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "ind60" & cfa_output_ex2$rhs == "ind60" & cfa_output_ex2$op == "~~", ]$est,
                   tspa_output_ex2[tspa_output_ex2$lhs == "ind60" & tspa_output_ex2$rhs == "ind60" & tspa_output_ex2$op == "~~", ]$est)
    })

    test_that("test if the se of variance is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "ind60" & cfa_output_ex2$rhs == "ind60" & cfa_output_ex2$op == "~~", ]$se,
                   tspa_output_ex2[tspa_output_ex2$lhs == "ind60" & tspa_output_ex2$rhs == "ind60" & tspa_output_ex2$op == "~~", ]$se)
    })

    test_that("test if the variance of factor is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "dem60" & cfa_output_ex2$rhs == "dem60" & cfa_output_ex2$op == "~~", ]$est,
                   tspa_output_ex2[tspa_output_ex2$lhs == "dem60" & tspa_output_ex2$rhs == "dem60" & tspa_output_ex2$op == "~~", ]$est)
    })

    test_that("test if the se of variance is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "dem60" & cfa_output_ex2$rhs == "dem60" & cfa_output_ex2$op == "~~", ]$se,
                   tspa_output_ex2[tspa_output_ex2$lhs == "dem60" & tspa_output_ex2$rhs == "dem60" & tspa_output_ex2$op == "~~", ]$se)
    })

    test_that("test if the variance of factor is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "dem65" & cfa_output_ex2$rhs == "dem65" & cfa_output_ex2$op == "~~", ]$est,
                   tspa_output_ex2[tspa_output_ex2$lhs == "dem65" & tspa_output_ex2$rhs == "dem65" & tspa_output_ex2$op == "~~", ]$est)
    })

    test_that("test if the se of variance is the same for two methods", {
      expect_equal(cfa_output_ex2[cfa_output_ex2$lhs == "dem65" & cfa_output_ex2$rhs == "dem65" & cfa_output_ex2$op == "~~", ]$se,
                   tspa_output_ex2[tspa_output_ex2$lhs == "dem65" & tspa_output_ex2$rhs == "dem65" & tspa_output_ex2$op == "~~", ]$se)
    })

    # Fit measures

    cfa_fitmeasures_ex2 <- as.data.frame(fitmeasures(cfa_fit_ex2))
    tspa_fitmeasures_ex2 <- as.data.frame(fitmeasures(tspa_ex2))

    test_that("test if chisq is the same for two methods", {
      expect_equal(cfa_fitmeasures_ex2["chisq", ], tspa_fitmeasures_ex2["chisq", ])
      # We can add more comparisons of fitting measures by changing the name
    })
