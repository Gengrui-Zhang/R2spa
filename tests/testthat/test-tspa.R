########################################## Test 2S-PA function ##########################################

########################### Test 2S-PA function ##############################

# Loading packages and functions
library(lavaan)

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

    # tspa model
    tspa_single <-
      tspa(
        model = "dem60 ~ ind60",
        data = fs_dat_single,
        se = c(ind60 = 0.1213615, dem60 = 0.6756472)
      )

########## Testing section ############

    # Class of input
    var_len <- 2
    se = c(ind60 = 0.1213615, dem60 = 0.6756472)

    # The tspa data should be composed of two parts: variable, and se
    test_that("test the number of columns in tspa data are multiples of the variable length",
              {
                # HL: use x %% y == 0 to test whether x is a multiple of y
                # JZ: Oh nice technique! Thanks!
                expect_gt(ncol(fs_dat_single), 1)
                expect_equal(ncol(fs_dat_single) %% var_len, 0)
              })

    test_that ("Test the data variable names should contain prefix (fs_)", {
      fs_names <- colnames(fs_dat_single)  # HL: use small letters for variables
      # HL: alternative to test all is TRUE
      expect_true(all(grepl("fs_", fs_names)))
    })

    # Class of output

    # Parameter estimates

    # HL: Do not override an output with the same name!
    # JZ: Thanks for the reminder!

    test_that("test if the regression coefficients of factors are the same for two methods",
              {
                # HL: A more efficient way with `coef()`
                expect_equal(coef(cfa_single), coef(tspa_single))
              })

    test_that("test if the se of regression coefficients are the same for two methods",
              {
                # HL: Use `vcov()`
                # JZ: I have one question here. Do equal vcov() results mean the standard error
                #     and regression coefficients are equal for two methods? I believe this is because
                #     regression coefficents and se are calculated from var/cov matrix?
                expect_equal(vcov(cfa_single), vcov(tspa_single))
              })

    # Fit measures

    # HL: Can test all with fitmeasures()
    # JZ: Thanks for letting me know!

    test_that("test if fit indices are the same for two methods", {
      expect_equal(fitmeasures(cfa_single), fitmeasures(tspa_single))
      # We can add more comparisons of fitting measures by changing the name
    })

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
    # JZ: I see. Thanks for correcting the model.

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

    test_that("test if the se of regression coefficients are similar for two methods",
              {
                expect_lt(
                  max(abs(sem_path_3var$se - tspa_path_3var$se)),
                  expected = .01
                )
              })

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

    test_that("test if the se of variance is similar for two methods", {
      expect_lt(
        max(abs(sem_var_3var$se - tspa_var_3var$se)),
        expected = .01
      )
    })

########## Multi-group example ##########

    # get factor scores
    fs_dat_visual <- get_fs(data = HolzingerSwineford1939,
                            model = "visual =~ x1 + x2 + x3",
                            group = "school")
    fs_dat_speed <- get_fs(data = HolzingerSwineford1939,
                           model = "speed =~ x7 + x8 + x9",
                           group = "school")
    fs_dat_multi <- cbind(fs_dat_visual, fs_dat_speed)

    # SEM model
    sem_model_multi <- '
                            # latent variables (indicated by factor scores)
                              visual =~ c(1, 1) * fs_visual
                              speed =~ c(1, 1) * fs_speed
                            # constrain the errors
                              fs_visual ~~ c(0.11501092038276, 0.097236701584) * fs_visual
                              fs_speed ~~ c(0.07766672265625, 0.07510378617049) * fs_speed
                            # latent variances
                              visual ~~ c(v11, v12) * visual
                              speed ~~ c(v21, v22) * speed
                            # regressions
                              visual ~ speed
                           '

    sem_multi <-
      sem(model = sem_model_multi,
          data  = fs_dat_multi,
          group = "school")

    # tspa model
    tspa_multi <- tspa(
      model = "visual ~ speed",
      data = fs_dat_multi,
      se = data.frame(
        visual = c(0.3391326, 0.3118280),
        speed = c(0.2786875, 0.2740507)
      ),
      group = "school"
      # group.equal = "regressions"
    )

########## Testing section #############

    # Standardized parameter estimates
    sem_path_multi <- subset(standardizedSolution(sem_multi),
                            subset = op == "~")
    tspa_path_multi <- subset(standardizedSolution(tspa_multi),
                             subset = op == "~")

    test_that("test if the regression coefficients of factors are similar for two methods",
              {
                # HL: The two models are the same; use expect_equal()
                expect_equal(
                  sem_path_multi$est.std,
                  tspa_path_multi$est.std
                )
              })

    test_that("test if the se of regression coefficients are similar for two methods",
              {
                expect_equal(
                  sem_path_multi$se,
                  tspa_path_multi$se
                )
              })

    # Variance of factors

    # HL: Shouldn't they be `sem_multi` and `tspa_multi`?
    sem_var_multi <- subset(standardizedSolution(sem_multi),
                             subset = op == "~~" &
                             lhs %in% c("ind60", "dem60", "dem65"))
    tspa_var_multi <- subset(standardizedSolution(tspa_multi),
                              subset = op == "~~" &
                              lhs %in% c("ind60", "dem60", "dem65"))

    test_that("test if the variance of factor is similar for two methods", {
      expect_equal(
        sem_var_multi$est.std,
        tspa_var_multi$est.std
      )
    })

    test_that("test if the se of variance is similar for two methods", {
      expect_equal(
        sem_var_multi$se,
        tspa_var_multi$se
      )
    })
