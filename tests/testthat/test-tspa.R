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
            expect_gt(ncol(fs_dat_single), 1)
            expect_equal(ncol(fs_dat_single) %% var_len, 0)
          })

test_that ("Test the data variable names should contain prefix (fs_)", {
  fs_names <- colnames(fs_dat_single)
  expect_true(all(grepl("fs_", fs_names)))
})

# Class of output

# Parameter estimates

test_that("test if the regression coefficients of factors are the same for two methods",
          {
            expect_equal(coef(cfa_single)["dem60~ind60"],
                         coef(tspa_single)["dem60~ind60"])
          })

test_that("test if the se of regression coefficients are the same for two methods",
          {
            expect_equal(
              vcov(cfa_single)[c("dem60~ind60", "v1", "v2"),
                               c("dem60~ind60", "v1", "v2")],
              vcov(tspa_single)[c("dem60~ind60", "ind60~~ind60", "dem60~~dem60"),
                                c("dem60~ind60", "ind60~~ind60", "dem60~~dem60")],
              ignore_attr = TRUE
            )
          })

# Fit measures

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
fs_dat_multi <- cbind(
  do.call(rbind, fs_dat_visual),
  do.call(rbind, fs_dat_speed)
)

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

# # Test tspaSingleGroupMF()
# cfa_3fac <-  '
#   # latent variables
#   ind60 =~ x1 + x2 + x3
#   dem60 =~ y1 + y2 + y3 + y4
#   dem65 =~ y5 + y6 + y7 + y8
# '
# fs_dat_3fac <- get_fs(PoliticalDemocracy, model = cfa_3fac, std.lv = TRUE)
# path_mod <- '
# dem60 ~ ind60
# dem65 ~ ind60 + dem60
# '
# tspa_mod_s <- tspaSingleGroupMF(
#   model = path_mod,
#   data = fs_dat_3fac,
#   vc = attr(fs_dat_3fac, "av_efs"),
#   cross_loadings = attr(fs_dat_3fac, "fsA")
# )
#
# factors_order_s <- subset(lavaan::lavaanify(tspa_mod_s), op == "~")
# loadings_order_s <- subset(lavaan::lavaanify(tspa_mod_s), op == "=~")
#
# test_that("The order of factors in the model from tspaSingleGroupMF()", {
#   expect_equal(c("dem60", "dem65", "dem65"), factors_order_s$lhs)
#   expect_equal(c("ind60", "ind60", "dem60"), factors_order_s$rhs)
# })
# test_that("The order of loadings in the model from tspaSingleGroupMF()", {
#   expect_equal(rep(c("ind60", "dem60", "dem65"), each = 3),
#                loadings_order_s$lhs)
#   expect_equal(rep(c("fs_ind60", "fs_dem60", "fs_dem65"), 3),
#                loadings_order_s$rhs)
# })


# Test tspaMultipleGroupMF()
mod4 <- "
  # latent variables
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
"
fs_dat4 <- get_fs(HolzingerSwineford1939, model = mod4, std.lv = TRUE,
                  group = "school")
tspa_mod_m <- tspaMultipleGroupMF(
  model = "visual ~ speed
           textual ~ visual + speed",
  data = fs_dat4,
  vc = attr(fs_dat4, "av_efs"),
  cross_loadings = attr(fs_dat4, "fsA")
)

factors_order_m <- subset(lavaan::lavaanify(tspa_mod_m, ngroup = 2),
                          op == "~")
loadings_order_m <- subset(lavaan::lavaanify(tspa_mod_m, ngroup = 2),
                           op == "=~")

test_that("The order of factors in the model from tspaSingleGroupMF()", {
  expect_equal(rep(c("visual", "textual", "textual"), 2),
               factors_order_m$lhs)
  expect_equal(rep(c("speed", "visual", "speed"), 2),
               factors_order_m$rhs)
})
test_that("The order of loadings in the model from tspaSingleGroupMF()", {
  expect_equal(rep(c("visual", "textual", "speed"), each = 3) |> rep(2),
               loadings_order_m$lhs)
  expect_equal(rep(c("fs_visual", "fs_textual", "fs_speed"), 6),
               loadings_order_m$rhs)
})

# Compare results to using Bartlett's scores
tspa_fit_m <- tspa(
  model = "visual ~ speed
           textual ~ visual + speed",
  data = fs_dat4,
  group = "school",
  vc = attr(fs_dat4, "av_efs"),
  cross_loadings = attr(fs_dat4, "fsA")
)
fs_dat4b <- get_fs(HolzingerSwineford1939, model = mod4,
                   group = "school", method = "Bartlett")
sem_fit_m <- sem(
  model = "visual =~ fs_visual
           speed =~ fs_speed
           textual =~ fs_textual
           fs_visual ~~ c(0.2633962, 0.2827317) * fs_visual
           fs_textual ~~ c(0.1239827, 0.1282725) * fs_textual
           fs_speed ~~ c(0.2020107, 0.1332701) * fs_speed
           visual ~ speed
           textual ~ visual + speed",
  data = do.call(rbind, fs_dat4b),
  group = "school"
)

test_that("Multiple-group multiple-factor example", code = {
  sct <- standardizedSolution(tspa_fit_m)
  scs <- standardizedSolution(sem_fit_m)
  expect_equal(sct$est[sct$op == "~"], expected = scs$est[scs$op == "~"],
               tolerance = 0.0001)
  expect_equal(sct$se[sct$op == "~"], expected = scs$se[scs$op == "~"],
               tolerance = 0.0001)
})

# Test growth model
pop_growth_mod <- "
eta1 =~ .8 * x1 + .6 * y1 + .4 * z1
eta2 =~ .85 * x2 + .65 * y2 + .45 * z2
eta3 =~ .9 * x3 + .7 * y3 + .5 * z3
eta4 =~ .95 * x4 + .75 * y4 + .55 * z4

eta1 ~ 0 * 1
eta2 ~ 0 * 1
eta3 ~ 0 * 1
eta4 ~ 0 * 1

# covariances
x1 ~~ .8 * x2 + .6 * x3 + .4 * x4
x2 ~~ .8 * x3 + .6 * x4
x3 ~~ .8 * x4
y1 ~~ .8 * y2 + .6 * y3 + .4 * y4
y2 ~~ .8 * y3 + .6 * y4
y3 ~~ .8 * y4
z1 ~~ .8 * z2 + .6 * z3 + .4 * z4
z2 ~~ .8 * z3 + .6 * z4
z3 ~~ .8 * z4

i =~ 1 * eta1 + 1 * eta2 + 1 * eta3 + 1 * eta4
s =~ 0 * eta1 + 1 * eta2 + 2 * eta3 + 3 * eta4

i ~~ 1 * i
s ~~ 1 * s
i ~~ .3 * s
i ~ 0 * 1
s ~ 0 * 1
"
set.seed(1234)
growth_dat <- simulateData(pop_growth_mod, sample.nobs = 1000)

mod5 <- "
eta1 =~ x1 + y1 + z1
eta2 =~ x2 + y2 + z2
eta3 =~ x3 + y3 + z3
eta4 =~ x4 + y4 + z4

# covariances
x1 ~~ x2 + x3 + x4
x2 ~~ x3 + x4
x3 ~~ x4
y1 ~~ y2 + y3 + y4
y2 ~~ y3 + y4
y3 ~~ y4
z1 ~~ z2 + z3 + z4
z2 ~~ z3 + z4
z3 ~~ z4
"
fs_dat5 <- get_fs(growth_dat, model = mod5, std.lv = TRUE)
growth_mod <- "
i =~ 1 * eta1 + 1 * eta2 + 1 * eta3 + 1 * eta4
s =~ 0 * eta1 + 1 * eta2 + 2 * eta3 + 3 * eta4

i ~~ i
s ~~ s
i ~~ s
i ~ 1
s ~ 1

eta1 ~ 0 * 1
eta2 ~ 0 * 1
eta3 ~ 0 * 1
eta4 ~ 0 * 1
"
growth_fit <- tspa(growth_mod, fs_dat5,
                   vc = attr(fs_dat5, "av_efs"),
                   cross_loadings = attr(fs_dat5, "fsA"),
                   fsb = attr(fs_dat5, "fsb"))
