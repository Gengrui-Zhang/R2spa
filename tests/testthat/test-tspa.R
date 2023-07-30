########################### Test 2S-PA function ##############################

# Loading packages and functions
library(lavaan)
library(OpenMx)
library(umx)

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
    se_fs = c(ind60 = 0.1213615, dem60 = 0.6756472)
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
  se_fs = c(
    ind60 = 0.1213615,
    dem60 = 0.6756472,
    dem65 = 0.5724405
  )
)

# Compare to Mx
model_umx <- umxLav2RAM("
  fs_dem60 ~ fs_ind60
  fs_dem65 ~ fs_ind60 + fs_dem60
  fs_dem65 + fs_dem60 + fs_ind60 ~ 1
  ", printTab = FALSE)
# Loading
matL <- mxMatrix(
  type = "Iden", nrow = 3,
  free = FALSE,
  name = "L"
)
# Error
matE <- mxMatrix(
  type = "Diag", nrow = 3, ncol = 3,
  free = FALSE,
  values = c(0.6756472, 0.5724405, 0.1213615)^2,
  name = "E"
)
tspa_mx <- tspa_mx_model(model_umx, data = fs_dat_3var,
                         mat_ld = matL, mat_vc = matE)
tspa_mx_fit <- mxRun(tspa_mx)
# Check same coefficients and standard errors
test_that("test same regression coefficients with Mx", {
  expect_equal(
    coef(tspa_mx_fit)[c(2, 3, 1, 6, 4, 5)],
    expected = coef(tspa_3var),
    tolerance = 1e-5,
    ignore_attr = TRUE
  )
})
test_that("test same standard errors with Mx", {
  vc_mx <- diag(vcov(tspa_mx_fit))
  vc_lavaan <- diag(vcov(tspa_3var))
  expect_equal(
    vc_mx[c(2, 3, 1, 6, 4, 5)],
    expected = vc_lavaan,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
# Use numeric matrices
tspa_mx2 <- tspa_mx_model(model_umx,
  data = fs_dat_3var,
  mat_ld = diag(3) |>
    `dimnames<-`(list(
      c("fs_ind60", "fs_dem60", "fs_dem65"),
      c("ind60", "dem60", "dem65")
    )),
  mat_vc = diag(c(0.1213615, 0.6756472, 0.5724405)^2) |>
    `dimnames<-`(rep(list(c("fs_ind60", "fs_dem60", "fs_dem65")), 2))
)
tspa_mx_fit2 <- mxRun(tspa_mx2)
# Use column names for VC
err_cov <- matrix(c("ev_fs_ind60", NA, NA,
                    NA, "ev_fs_dem60", NA,
                    NA, NA, "ev_fs_dem65"), nrow = 3) |>
    `dimnames<-`(rep(list(c("fs_ind60", "fs_dem60", "fs_dem65")), 2))
tspa_mx3 <- tspa_mx_model(model_umx, data = fs_dat_3var,
  mat_ld = matL, mat_vc = err_cov)
tspa_mx_fit3 <- mxRun(tspa_mx3)
test_that("Same results with different Mx matrices input", {
  expect_equal(
    coef(tspa_mx_fit2),
    expected = coef(tspa_mx_fit)
  )
  expect_equal(
    coef(tspa_mx_fit3),
    expected = coef(tspa_mx_fit),
    tolerance = 1e-5
  )
})

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
  se_fs = data.frame(
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

# Test tspaSingleGroupMF()
cfa_3fac <-  '
  # latent variables
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
'
fs_dat_3fac <- get_fs(PoliticalDemocracy, model = cfa_3fac, std.lv = TRUE)
path_mod <- '
dem60 ~ ind60
dem65 ~ ind60 + dem60
'
tspa_mod_s <- tspaSingleGroupMF(
  model = path_mod,
  data = fs_dat_3fac,
  vc = attr(fs_dat_3fac, "av_efs"),
  cross_loadings = attr(fs_dat_3fac, "fsA")
)

factors_order_s <- subset(lavaan::lavaanify(tspa_mod_s), op == "~")
loadings_order_s <- subset(lavaan::lavaanify(tspa_mod_s), op == "=~")

test_that("The order of factors in the model from tspaSingleGroupMF()", {
  expect_equal(c("dem60", "dem65", "dem65"), factors_order_s$lhs)
  expect_equal(c("ind60", "ind60", "dem60"), factors_order_s$rhs)
})
test_that("The order of loadings in the model from tspaSingleGroupMF()", {
  expect_equal(rep(c("ind60", "dem60", "dem65"), each = 3),
               loadings_order_s$lhs)
  expect_equal(rep(c("fs_ind60", "fs_dem60", "fs_dem65"), 3),
               loadings_order_s$rhs)
})


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
