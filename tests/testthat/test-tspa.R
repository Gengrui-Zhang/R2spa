library(lavaan)
my_cfa <- '
   # latent variables
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
'
cfa_fit <- cfa(model = my_cfa,
               data  = PoliticalDemocracy)
fs_dat <- lavPredict(cfa_fit, se = "standard")
colnames(fs_dat) <- c("fs_ind60", "fs_dem60")

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
