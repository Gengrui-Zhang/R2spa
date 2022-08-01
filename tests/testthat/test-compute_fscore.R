library(lavaan)

test_that("compute_fscore() gives same scores as lavaan::lavPredict()", {
  fit <- cfa(" ind60 =~ x1 + x2 + x3 ",
             data = PoliticalDemocracy, std.lv = TRUE)
  fs_lavaan1 <- lavPredict(fit, method = "regression")
  fs_lavaan2 <- lavPredict(fit, method = "Bartlett")
  est <- lavInspect(fit, what = "est")
  fs_hand1 <- compute_fscore(lavInspect(fit, what = "data"),
                             lambda = est$lambda,
                             theta = est$theta,
                             psi = est$psi)
  fs_hand2 <- compute_fscore(lavInspect(fit, what = "data"),
                             lambda = est$lambda,
                             theta = est$theta,
                             psi = est$psi,
                             method = "Bartlett")
  expect_equal(unclass(fs_lavaan1), unclass(fs_hand1))
  expect_equal(unclass(fs_lavaan2), unclass(fs_hand2))
})

test_that("compute_fscore() works for multiple factors", {
  fit <- cfa(
    " ind60 =~ x1 + x2 + x3
      dem60 =~ y1 + y2 + y3 + y4 ",
    data = PoliticalDemocracy)
  fs_lavaan1 <- lavPredict(fit, method = "regression",
                           acov = "standard")
  est <- lavInspect(fit, what = "est")
  fs_hand1 <- compute_fscore(lavInspect(fit, what = "data"),
                             lambda = est$lambda,
                             theta = est$theta,
                             psi = est$psi,
                             acov = TRUE)
  expect_equal(fs_lavaan1, fs_hand1, ignore_attr = TRUE)
  expect_equal(attr(fs_lavaan1, "acov")[[1]],
               attr(fs_hand1, "acov"))
})

test_that("ACOV = Var(e.fs) for Bartlett scores", {
  fit <- cfa(
    " ind60 =~ x1 + x2 + x3
      dem60 =~ y1 + y2 + y3 + y4
      dem65 =~ y5 + y6 + y7 + y8 ",
    data = PoliticalDemocracy)
  fs_lavaan2 <- lavPredict(fit, method = "Bartlett",
                           acov = "standard")
  est <- lavInspect(fit, what = "est")
  fs_hand2 <- compute_fscore(lavInspect(fit, what = "data"),
                             lambda = est$lambda,
                             theta = est$theta,
                             psi = est$psi,
                             acov = TRUE,
                             method = "Bartlett",
                             fs_matrices = TRUE)
  expect_equal(fs_lavaan2, fs_hand2, ignore_attr = TRUE)
  expect_equal(attr(fs_lavaan2, "acov")[[1]],
               attr(fs_hand2, "acov"))
  expect_equal(attr(fs_lavaan2, "acov")[[1]],
               attr(fs_hand2, "av_efs"))
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
