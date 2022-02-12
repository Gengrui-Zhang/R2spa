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
