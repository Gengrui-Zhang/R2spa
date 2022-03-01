library(lavaan)
# example 1
my_cfa <- '
   # latent variables
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
'
cfa_fit <- cfa(model = my_cfa,
               data  = PoliticalDemocracy)
fs_dat <- lavPredict(cfa_fit, se = "standard")
colnames(fs_dat) <- c("fs_ind60", "fs_dem60")

# example 2
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
cfa_3var_fit <- cfa(model = cfa_3var,
                    data  = PoliticalDemocracy)
fs_3var_dat <- lavPredict(cfa_3var_fit, se = "standard")
colnames(fs_3var_dat) <- c("fs_ind60", "fs_dem60", "fs_dem65")
# tspa(model = "dem60 ~ ind60
#               dem65 ~ ind60 + dem60",
#      data = fs_dat,
#      reliability = c(ind60 = 0.9968282, dem60 = 0.8503460, dem65 = 0.8430526))


# tests
test_that("example in the vignette", {
  expect_equal(round(
    parameterEstimates(
      tspa(model = "dem60 ~ ind60",
           data = fs_dat,
           reliability = c(ind60 = 0.9651282, dem60 = 0.9055203))
      )[3, "est"],
    3),
    .015
  )
})
