library(lavaan)

test_that("compute_fscore() gives same scores as lavaan::lavPredict()", {
  fit <- cfa(" ind60 =~ x1 + x2 + x3 ",
             data = PoliticalDemocracy, std.lv = TRUE)
  fs_lavaan <- lavPredict(fit, method = "regression")
  est <- lavInspect(fit, what = "est")
  fs_hand <- compute_fscore(lavInspect(fit, what = "data"),
                            lambda = est$lambda,
                            theta = est$theta,
                            psi = est$psi)
  expect_equal(unclass(fs_lavaan), unclass(fs_hand))
})

test_that("compute_fscore() works for multiple factors", {
  fit <- cfa(
    " ind60 =~ x1 + x2 + x3
      dem60 =~ y1 + y2 + y3 + y4 ",
    data = PoliticalDemocracy)
  fs_lavaan <- lavPredict(fit, method = "regression",
                          acov = "standard")
  est <- lavInspect(fit, what = "est")
  fs_hand <- compute_fscore(lavInspect(fit, what = "data"),
                            lambda = est$lambda,
                            theta = est$theta,
                            psi = est$psi,
                            acov = TRUE)
  expect_equal(fs_lavaan, fs_hand, ignore_attr = TRUE)
  expect_equal(attr(fs_lavaan, "acov")[[1]],
               attr(fs_hand, "acov"))
})

test_that("Same factor scores with same priors", {
  hs_model <- "
  visual  =~ x1 + x2 + x3
  "
  fit <- cfa(hs_model,
             data = HolzingerSwineford1939,
             group = "school",
             group.equal = c("loadings", "intercepts", "residuals"))
  two_cases <-
    c(which(subset(HolzingerSwineford1939, subset = school == "Pasteur",
                   select = id) == 47),
      which(subset(HolzingerSwineford1939, subset = school == "Grant-White",
                   select = id) == 275))
  fs_lavaan <- lavPredict(fit, method = "regression",
                          acov = "standard")
  # Different factor scores in lavaan
  expect_false(fs_lavaan[[1]][two_cases[1], ] ==
                 fs_lavaan[[2]][two_cases[2], ])
  est <- lavInspect(fit, what = "est")
  y <- lavInspect(fit, what = "data")
  fs1_hand <- compute_fscore(y[[1]],
                             lambda = est[[1]]$lambda,
                             theta = est[[1]]$theta,
                             psi = est[[1]]$psi,
                             nu = est[[1]]$nu,
                             alpha = est[[1]]$alpha,
                             acov = TRUE)
  fs2_hand <- compute_fscore(y[[2]],
                             lambda = est[[2]]$lambda,
                             theta = est[[2]]$theta,
                             psi = est[[1]]$psi,
                             nu = est[[2]]$nu,
                             alpha = est[[1]]$alpha,
                             acov = TRUE)
  expect_equal(fs1_hand[two_cases[1], ], fs2_hand[two_cases[2], ])
})
