########################### Test 2S-PA function ##############################
# Loading packages and functions
library(lavaan)
# devtools::load_all()  # HL: this is not needed in a test context

########## Single-group example ##########

# Prepare test objects

# Example 1: Single-group with two variables

    # CFA model
    cfa_single1 <- '
                            # latent variables
                            ind60 =~ x1 + x2 + x3
                           '
    cfa_single2 <- '
                            # latent variables
                            dem60 =~ y1 + y2 + y3 + y4
                           '

    # get factor scores
    fs_single1 <- get_fs(PoliticalDemocracy, cfa_single1)
    fs_single2 <- get_fs(PoliticalDemocracy, cfa_single2)
    fs_dat_single <- cbind(fs_single1, fs_single2)

    # HL: SE numbers changed with new get_fs()

    cfa_model_single <- '
                                 # latent variables (indicated by factor scores)
                                   ind60 =~ 1 * fs_ind60
                                   dem60 =~ 1 * fs_dem60
                                 # constrain the errors
                                   fs_ind60 ~~ 0.1213615^2 * fs_ind60
                                   fs_dem60 ~~ 0.6756472^2 * fs_dem60
                                 # latent variances
                                   ind60 ~~ v1 * ind60
                                   dem60 ~~ v2 * dem60
                                 # regressions
                                   dem60 ~ ind60
                              '
    cfa_single <-
      sem(model = cfa_model_single, data  = fs_dat_single)
    # cfa_output_single <- summary(cfa_single)  # HL: no longer needed

    # tspa model
    tspa_single <-
      tspa(
        model = "dem60 ~ ind60",
        data = fs_dat_single,
        se = c(ind60 = 0.1213615, dem60 = 0.6756472)
      )
    # tspa_output_single <- summary(tspa_single)  # HL: no longer needed

########## Testing section ############

    # Class of input
    var_len <- 2
    se = c(ind60 = 0.1213615, dem60 = 0.6756472)

    # The tspa data should be composed of two parts: variable, and se
    test_that("test the number of columns in tspa data are multiples of the variable length",
              {
                # HL: use x %% y == 0 to test whether x is a multiple of y
                expect_gt(ncol(fs_dat_single), 1)
                expect_equal(ncol(fs_dat_single) %% var_len, 0)
              })

    # test_that("test if the length of se is the same as the length of variable", {
    #   expect_equal(var_len, length(se))
    # }) # HL: This doesn't seem needed as both are created by us in this file

    # test_that ("Test the data length is larger than 1", {
    #   expect_gt(ncol(fs_dat_single), 1)
    # }) # HL: suggest to combine with a previous test

    test_that ("Test the data variable names should contain prefix (fs_)", {
      fs_names <- colnames(fs_dat_single)  # HL: use small letters for variables
      # HL: alternative to test all is TRUE
      expect_true(all(grepl("fs_", fs_names)))
    })

    # Class of output

    # Parameter estimates

    # HL: Do not override an output with the same name!
    # cfa_output_single <- as.data.frame(cfa_output_single$pe)
    # tspa_output_single <- as.data.frame(tspa_output_single$pe)

    test_that("test if the regression coefficients of factors are the same for two methods",
              {
                # HL: A more efficient way with `coef()`
                expect_equal(coef(cfa_single), coef(tspa_single))
              })

    test_that("test if the se of regression coefficients are the same for two methods",
              {
                # HL: Use `vcov()`
                expect_equal(vcov(cfa_single), vcov(tspa_single))
              })

    # Variances of factors

    # HL: already tested before
    # test_that("test if the variance of factor is the same for two methods", {
    #   expect_equal(cfa_output_single[cfa_output_single$lhs == "ind60" &
    #                                    cfa_output_single$rhs == "ind60" &
    #                                    cfa_output_single$op == "~~",]$est,
    #                tspa_output_single[tspa_output_single$lhs == "ind60" &
    #                                     tspa_output_single$rhs == "ind60" &
    #                                     tspa_output_single$op == "~~",]$est)
    # })

    # HL: Same
    # test_that("test if the se of variance is the same for two methods", {
    #   expect_equal(cfa_output_single[cfa_output_single$lhs == "ind60" &
    #                                    cfa_output_single$rhs == "ind60" &
    #                                    cfa_output_single$op == "~~",]$se,
    #                tspa_output_single[tspa_output_single$lhs == "ind60" &
    #                                     tspa_output_single$rhs == "ind60" &
    #                                     tspa_output_single$op == "~~",]$se)
    # })

    # test_that("test if the variance of factor is the same for two methods", {
    #   expect_equal(cfa_output_single[cfa_output_single$lhs == "dem60" &
    #                                    cfa_output_single$rhs == "dem60" &
    #                                    cfa_output_single$op == "~~",]$est,
    #                tspa_output_single[tspa_output_single$lhs == "dem60" &
    #                                     tspa_output_single$rhs == "dem60" &
    #                                     tspa_output_single$op == "~~",]$est)
    # })

    # test_that("test if the se of variance is the same for two methods", {
    #   expect_equal(cfa_output_single[cfa_output_single$lhs == "dem60" &
    #                                    cfa_output_single$rhs == "dem60" &
    #                                    cfa_output_single$op == "~~",]$se,
    #                tspa_output_single[tspa_output_single$lhs == "dem60" &
    #                                     tspa_output_single$rhs == "dem60" &
    #                                     tspa_output_single$op == "~~",]$se)
    # })

    # Combined testing

    # test_that("test if there is any parameter difference less than 0.1", {
    #   expect_equal(max(abs(
    #     cfa_output_single[, "est"] - tspa_output_single[, "est"]
    #   )), 0)
    # })

    # test_that("test if all parameter differences are larger than 0.5", {
    #   expect_gte(max(abs(
    #     cfa_output_single[, "est"] - tspa_output_single[, "est"]
    #   )), 0.5)
    # })
    # Failed test 1

    # test_that("test if there is any se differnece less than 0.1", {
    #   expect_lte(max(abs(
    #     cfa_output_single[, "se"] - tspa_output_single[, "se"]
    #   )), 0.1)
    # })

    # test_that("test if all se differences are larger than 0.5", {
    #   expect_gte(max(abs(
    #     cfa_output_single[, "se"] - tspa_output_single[, "se"]
    #   )), 0.5)
    # })
    # Failed test 2

    # Fit measures

    # HL: Can test all with fitmeasures()
    # cfa_fitmeasures_single <- as.data.frame(fitmeasures(cfa_single))
    # tspa_fitmeasures_single <-
    #   as.data.frame(fitmeasures(tspa_single))

    test_that("test if fit indices are the same for two methods", {
      expect_equal(fitmeasures(cfa_single), fitmeasures(tspa_single))
      # We can add more comparisons of fitting measures by changing the name
    })

    # test_that("test if rmsea is the same for two methods", {
    #   expect_equal(cfa_fitmeasures_single["rmsea",], tspa_fitmeasures_single["rmsea",])
    #   # We can add more comparisons of fitting measures by changing the name
    # })

    # test_that("test if rmsea is the same for two methods", {
    #   expect_equal(cfa_fitmeasures_single["cfi",], tspa_fitmeasures_single["cfi",])
    #   # We can add more comparisons of fitting measures by changing the name
    # })

    # Combined tests
    # test_that("test if all fit measures differences are larger than 0.5", {
    #   expect_gte(max(abs(
    #     na.omit(cfa_fitmeasures_single) - na.omit(tspa_fitmeasures_single)
    #   )), 0.5)
    # })
    # Failed test 3

    # test_that("test if all fit measures differences are less than 0.1", {
    #   expect_lte(max(abs(
    #     na.omit(cfa_fitmeasures_single) - na.omit(tspa_fitmeasures_single)
    #   )), 0.1)
    # })

# Example 2: Single group with three variables

    # CFA model
    cfa_3var1 <- '
                            # latent variables
                            ind60 =~ x1 + x2 + x3
                           '
    cfa_3var2 <- '
                            # latent variables
                            dem60 =~ y1 + y2 + y3 + y4
                           '
    cfa_3var3 <- '
                            # latent variables
                            dem65 =~ y5 + y6 + y7 + y8
                           '

    # get factor scores
    fs_3var1 <- get_fs(PoliticalDemocracy, cfa_3var1)
    fs_3var2 <- get_fs(PoliticalDemocracy, cfa_3var2)
    fs_3var3 <- get_fs(PoliticalDemocracy, cfa_3var3)
    fs_dat_3var <- cbind(fs_3var1, fs_3var2, fs_3var3)

    # HL: I replace the example with a true SEM to avoid repeating the 2-var
    # example. Also call it sem as it has structural paths and not really CFA

    sem_model_3var <- '
                           # latent variables (indicated by factor scores)
                             ind60 =~ x1 + x2 + x3
                             dem60 =~ y1 + y2 + y3 + y4
                             dem65 =~ y5 + y6 + y7 + y8
                           # regressions
                             dem60 ~ ind60
                             dem65 ~ ind60 + dem60
                          '

    sem_3var <- sem(model = sem_model_3var, data  = PoliticalDemocracy)
    # cfa_output_3var <- summary(cfa_3var)  # HL: no longer needed

    # tspa model
    tspa_3var <- tspa(
      model = "dem60 ~ ind60
               dem65 ~ ind60 + dem60",
      data = fs_dat_3var,
      se = c(
        ind60 = 0.1213615,
        dem60 = 0.6756472,
        dem65 = 0.5724405
      )
    )
    # tspa_output_3var <- summary(tspa_3var)  # HL: no longer needed

########## Testing section #############

    # Standardized parameter estimates
    sem_path_3var <- subset(standardizedSolution(sem_3var),
                            subset = op == "~")
    tspa_path_3var <- subset(standardizedSolution(tspa_3var),
                             subset = op == "~")

    # HL: I used .05 as an arbitrary threshold
    test_that("test if the regression coefficients of factors are similar for two methods",
              {
                expect_lt(
                  max(abs(sem_path_3var$est.std - tspa_path_3var$est.std)),
                  expected = .05
                )
              })
    # Failed test 4
    test_that("test if the se of regression coefficients are similar for two methods",
              {
                expect_lt(
                  max(abs(sem_path_3var$se - tspa_path_3var$se)),
                  expected = .01
                )
              })
    # Failed test 5

    # HL: Already in the previous test
    # test_that("test if the regression coefficients of factors are the same for two methods",
    #           {
    #             expect_equal(cfa_output_3var[cfa_output_3var$lhs == "dem65" &
    #                                            cfa_output_3var$rhs == "ind60" &
    #                                            cfa_output_3var$op == "~",]$est,
    #                          tspa_output_3var[tspa_output_3var$lhs == "dem65" &
    #                                             tspa_output_3var$rhs == "ind60" &
    #                                             tspa_output_3var$op == "~",]$est)
    #           })
    # Failed test 6

    # test_that("test if the se of regression coefficient is the same for two methods",
    #           {
    #             expect_equal(cfa_output_3var[cfa_output_3var$lhs == "dem65" &
    #                                            cfa_output_3var$rhs == "ind60" &
    #                                            cfa_output_3var$op == "~",]$se,
    #                          tspa_output_3var[tspa_output_3var$lhs == "dem65" &
    #                                             tspa_output_3var$rhs == "ind60" &
    #                                             tspa_output_3var$op == "~",]$se)
    #           })
    # Failed test 7

    # test_that("test if the regression coefficient of factors is the same for two methods",
    #           {
    #             expect_equal(cfa_output_3var[cfa_output_3var$lhs == "dem65" &
    #                                            cfa_output_3var$rhs == "dem60" &
    #                                            cfa_output_3var$op == "~",]$est,
    #                          tspa_output_3var[tspa_output_3var$lhs == "dem65" &
    #                                             tspa_output_3var$rhs == "dem60" &
    #                                             tspa_output_3var$op == "~",]$est)
    #           })
    # Failed test 8

    # test_that("test if the se of regression coefficient is the same for two methods",
    #           {
    #             expect_equal(cfa_output_3var[cfa_output_3var$lhs == "dem65" &
    #                                            cfa_output_3var$rhs == "dem60" &
    #                                            cfa_output_3var$op == "~",]$se,
    #                          tspa_output_3var[tspa_output_3var$lhs == "dem65" &
    #                                             tspa_output_3var$rhs == "dem60" &
    #                                             tspa_output_3var$op == "~",]$se)
    #           })
    # Failed test 9

    # Combined testing
    # test_that("test if there is any parameter difference less than 0.1", {
    #   expect_lte(max(abs(cfa_output_3var[1:12, "est"] - tspa_output_3var[, "est"])), 0.1)
    # })
    # Failed test 10

    # test_that("test if all parameter differences are larger than 0.5", {
    #   expect_gte(max(abs(cfa_output_3var[1:12, "est"] - tspa_output_3var[, "est"])), 0.5)
    # })
    # Failed test 11

    # test_that("test if there is any se difference less than 0.1", {
    #   expect_lte(max(abs(cfa_output_3var[1:12, "se"] - tspa_output_3var[, "se"])), 0.1)
    # })

    # test_that("test if all se differences are larger than 0.5", {
    #   expect_gte(max(abs(cfa_output_3var[1:12, "se"] - tspa_output_3var[, "se"])), 0.5)
    # })
    # Failed test 12

    # Variance of factors
    sem_var_3var <- subset(standardizedSolution(sem_3var),
                           subset = op == "~~" &
                             lhs %in% c("ind60", "dem60", "dem65"))
    tspa_var_3var <- subset(standardizedSolution(tspa_3var),
                            subset = op == "~~" &
                              lhs %in% c("ind60", "dem60", "dem65"))

    test_that("test if the variance of factor is similar for two methods", {
      expect_lt(
        max(abs(sem_var_3var$est.std - tspa_var_3var$est.std)),
        expected = .05
      )
    })
    # Failed test 13

    test_that("test if the se of variance is similar for two methods", {
      expect_lt(
        max(abs(sem_var_3var$se - tspa_var_3var$se)),
        expected = .01
      )
    })
    # Failed test 14

    # HL: Already in previous test
    # test_that("test if the variance of factor is the same for two methods", {
    #   expect_equal(cfa_output_3var[cfa_output_3var$lhs == "dem60" &
    #                                  cfa_output_3var$rhs == "dem60" & cfa_output_3var$op == "~~",]$est,
    #                tspa_output_3var[tspa_output_3var$lhs == "dem60" &
    #                                   tspa_output_3var$rhs == "dem60" &
    #                                   tspa_output_3var$op == "~~",]$est)
    # })
    # Failed test 15

    # test_that("test if the se of variance is the same for two methods", {
    #   expect_equal(cfa_output_3var[cfa_output_3var$lhs == "dem60" &
    #                                  cfa_output_3var$rhs == "dem60" & cfa_output_3var$op == "~~",]$se,
    #                tspa_output_3var[tspa_output_3var$lhs == "dem60" &
    #                                   tspa_output_3var$rhs == "dem60" &
    #                                   tspa_output_3var$op == "~~",]$se)
    # })
    # Failed test 16

    # test_that("test if the variance of factor is the same for two methods", {
    #   expect_equal(cfa_output_3var[cfa_output_3var$lhs == "dem65" &
    #                                  cfa_output_3var$rhs == "dem65" & cfa_output_3var$op == "~~",]$est,
    #                tspa_output_3var[tspa_output_3var$lhs == "dem65" &
    #                                   tspa_output_3var$rhs == "dem65" &
    #                                   tspa_output_3var$op == "~~",]$est)
    # })
    # Failed test 17

    # test_that("test if the se of variance is the same for two methods", {
    #   expect_equal(cfa_output_3var[cfa_output_3var$lhs == "dem65" &
    #                                  cfa_output_3var$rhs == "dem65" & cfa_output_3var$op == "~~",]$se,
    #                tspa_output_3var[tspa_output_3var$lhs == "dem65" &
    #                                   tspa_output_3var$rhs == "dem65" &
    #                                   tspa_output_3var$op == "~~",]$se)
    # })
    # Failed test 18

    # Fit measures

    # HL: Not comparable due to the change in the comparison model to SEM
    # cfa_fitmeasures_3var <- as.data.frame(fitmeasures(cfa_3var))
    # tspa_fitmeasures_3var <- as.data.frame(fitmeasures(tspa_3var))

    # test_that("test if chisq is the same for two methods", {
    #   expect_equal(cfa_fitmeasures_3var["chisq",], tspa_fitmeasures_3var["chisq",])
    #   # We can add more comparisons of fitting measures by changing the name
    # })

    # Combined tests
    # test_that("test if all fit measures differences are larger than 0.5", {
    #   expect_gte(max(abs(
    #     na.omit(cfa_fitmeasures_3var) - na.omit(tspa_fitmeasures_3var)
    #   )), 0.5)
    # })
    # Failed test 19

    # test_that("test if all fit measures differences are less than 0.1", {
    #   expect_lte(max(abs(
    #     na.omit(cfa_fitmeasures_3var) - na.omit(tspa_fitmeasures_3var)
    #   )), 0.1)
    # })


########## Multi-group example ##########

    # get factor scores
    fs_dat_visual <- get_fs(data = HolzingerSwineford1939,
                            model = "visual =~ x1 + x2 + x3",
                            group = "school")
    fs_dat_speed <- get_fs(data = HolzingerSwineford1939,
                           model = "speed =~ x7 + x8 + x9",
                           group = "school")
    fs_dat_multi <- cbind(fs_dat_visual, fs_dat_speed)

    # CFA model
    cfa_model_multi <- '
                            # latent variables (indicated by factor scores)
                              visual=~ c(1, 1) * fs_visual
                              speed=~ c(1, 1) * fs_speed
                            # constrain the errors
                              fs_visual~~ c(c(0.17077035558969, 0.13909811731396)) * fs_visual
                              fs_speed~~ c(c(0.11726529057604, 0.09240590747556)) * fs_speed
                            # latent variances
                              visual ~~ c(v11, v12) * visual
                              speed ~~ c(v21, v22) * speed
                            # regressions
                              visual ~ speed
                           '

    cfa_multi <-
      sem(model = cfa_model_multi,
          data  = fs_dat_multi,
          group = "school")
    cfa_output_multi <- summary(cfa_multi)


    # tspa model
    tspa_multi <- tspa(
      model = "visual ~ speed",
      data = fs_dat_multi,
      se = data.frame(
        visual = c(0.4132437, 0.3729586),
        speed = c(0.3424402, 0.3039834)
      ),
      group = "school",
      group.equal = "regressions"
    )
    tspa_output_multi <- summary(tspa_multi)

########## Testing section #############

    # Parameter estimates

    cfa_output_multi <- as.data.frame(cfa_output_multi$pe)
    tspa_output_multi <- as.data.frame(tspa_output_multi$pe)

    # Combined tests

    test_that("test if there is any parameter difference less than 0.1", {
      expect_lte(max(abs(cfa_output_multi[, "est"] - tspa_output_multi[, "est"])), 0.1)
    })

    test_that("test if all parameter differences are larger than 0.5", {
      expect_gte(max(abs(cfa_output_multi[, "est"] - tspa_output_multi[, "est"])), 0.5)
    })
    # Failed test 20

    test_that("test if there is any se difference less than 0.1", {
      expect_lte(max(abs(cfa_output_multi[, "se"] - tspa_output_multi[, "se"])), 0.1)
    })
    # Failed test 21

    test_that("test if all se differences are larger than 0.5", {
      expect_gte(max(abs(cfa_output_multi[, "se"] - tspa_output_multi[, "se"])), 0.5)
    })
    # Failed test 22

    # Variance of factors

    test_that("test if the variance of factor is the same for two methods", {
      expect_equal(cfa_output_multi[cfa_output_multi$lhs == "visual" &
                                      cfa_output_multi$rhs == "visual" &
                                      cfa_output_multi$op == "~~",]$est,
                   tspa_output_multi[tspa_output_multi$lhs == "visual" &
                                       tspa_output_multi$rhs == "visual" &
                                       tspa_output_multi$op == "~~",]$est)
    })
    # Failed test 23

    test_that("test if the se of variance is the same for two methods", {
      expect_equal(cfa_output_multi[cfa_output_multi$lhs == "visual" &
                                      cfa_output_multi$rhs == "visual" &
                                      cfa_output_multi$op == "~~",]$se,
                   tspa_output_multi[tspa_output_multi$lhs == "visual" &
                                       tspa_output_multi$rhs == "visual" &
                                       tspa_output_multi$op == "~~",]$se)
    })
    # Failed test 24

    test_that("test if the variance of factor is the same for two methods", {
      expect_equal(cfa_output_multi[cfa_output_multi$lhs == "speed" &
                                      cfa_output_multi$rhs == "speed" &
                                      cfa_output_multi$op == "~~",]$est,
                   tspa_output_multi[tspa_output_multi$lhs == "speed" &
                                       tspa_output_multi$rhs == "speed" &
                                       tspa_output_multi$op == "~~",]$est)
    })
    # Failed test 25

    test_that("test if the se of variance is the same for two methods", {
      expect_equal(cfa_output_multi[cfa_output_multi$lhs == "speed" &
                                      cfa_output_multi$rhs == "speed" &
                                      cfa_output_multi$op == "~~",]$se,
                   tspa_output_multi[tspa_output_multi$lhs == "speed" &
                                       tspa_output_multi$rhs == "speed" &
                                       tspa_output_multi$op == "~~",]$se)
    })
    # Failed test 26

    # Fit measures
    cfa_fitmeasures_multi <- as.data.frame(fitmeasures(cfa_multi))
    tspa_fitmeasures_multi <- as.data.frame(fitmeasures(tspa_multi))

    test_that("test if chisq is the same for two methods", {
      expect_equal(cfa_fitmeasures_multi["chisq",], tspa_fitmeasures_multi["chisq",])
      # We can add more comparisons of fitting measures by changing the name
    })
    # Failed test 27

    # Combined tests
    test_that("test if all fit measures differences are larger than 0.5", {
      expect_gte(max(abs(
        na.omit(cfa_fitmeasures_multi - tspa_fitmeasures_multi)
      )), 0.5)
    })

    test_that("test if all fit measures differences are less than 0.1", {
      expect_lte(max(abs(
        na.omit(cfa_fitmeasures_multi - tspa_fitmeasures_multi)
      )), 0.1)
    })
    # Failed test 28
