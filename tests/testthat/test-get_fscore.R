# Loading packages and functions
library(lavaan)

########## Single-group example ##########

# Prepare test objects
single_model <- '
                 # latent variables
                   ind60 =~ x1 + x2 + x3
                   dem60 =~ y1 + y2 + y3 + y4
                '

# model = cfa_3var
test_object_fs <- get_fs(PoliticalDemocracy, single_model)

########## Testing section ############

# Class of input

test_that("test the model input", {
  expect_type(single_model, "character")
})

test_that("test the data input", {
  expect_s3_class(PoliticalDemocracy, "data.frame")
})

# Class of output

test_that("Gives an output of data frame", {
  expect_s3_class(test_object_fs, "data.frame")
})

ncol_cfa <- function(x) {
  nfac <- nrow(lavInspect(x, what = "cor.lv"))
  # fs + se + loadings + ev
  nfac * 2 + nfac ^ 2 + nfac * (nfac + 1) / 2
}

test_that("Test the number of factors is equal", {
  expected_cols <- ncol_cfa(
    cfa(model = single_model, data = PoliticalDemocracy)
  )
  expect_equal(length(test_object_fs), expected_cols)
})

test_that("Number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs), nrow(PoliticalDemocracy))
})

# Test standard error

test_that("Standard errors for each observation are the same", {
  fs_names <- colnames(test_object_fs)
  names_se <- grep("_se", fs_names, value = TRUE)
  for (j in names_se) {
    expect_identical(var(test_object_fs[[j]]), 0)
  }
})

test_that(
  "Standard errors for each observation are positive numbers and within 1",
  {
    fs_names <- colnames(test_object_fs)
    names_se <- grep("_se", fs_names, value = TRUE)
    for (j in names_se) {
      expect_gt(min(test_object_fs[[j]]), 0)
      expect_lt(max(test_object_fs[[j]]), 1)
    }
  }
)

# Bartlett scores
test_object_fs_bar <- get_fs(
  PoliticalDemocracy, single_model,
  method = "Bartlett"
)

test_that("Standard errors for each observation are the same", {
  fs_names <- colnames(test_object_fs_bar)
  names_se <- grep("_se", fs_names, value = TRUE)
  for (j in names_se) {
    expect_identical(var(test_object_fs_bar[[j]]), 0)
  }
})

test_that(
  "Standard errors for each observation are positive numbers and within 1",
  {
    fs_names <- colnames(test_object_fs)
    names_se <- grep("_se", fs_names, value = TRUE)
    for (j in names_se) {
      expect_gt(min(test_object_fs_bar[[j]]), 0)
      expect_lt(max(test_object_fs_bar[[j]]), 1)
    }
  }
)


########## multi-group examples ##########

###### One-factor example #####

# Prepare for test objects
hs_model <- 'visual  =~ x1 + x2 + x3'
# multi_fit <- cfa(hs_model,
#                  data = HolzingerSwineford1939,
#                  group = "school")
#get_fs(HolzingerSwineford1939, hs_model, group = "school")
test_object_fs_multi <- get_fs(
  HolzingerSwineford1939[c("school", "x1", "x2", "x3")],
  hs_model,
  group = "school"
)

########## Testing section ############

# Class of output

test_that("Number of factors is equal", {
  expected_cols <- ncol_cfa(
    cfa(model = hs_model, data = PoliticalDemocracy)
  )
  # Add 1 for group
  expect_equal(length(test_object_fs_multi[[1]]), expected_cols + 1)
})

test_that("Number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs_multi[[1]]) +
                     nrow(test_object_fs_multi[[2]]),
                   nrow(HolzingerSwineford1939))
})

# Test standard errors

test_that(
  "Standard errors for each observation are the same within groups",
  {
    test_se <- vapply(test_object_fs_multi,
      FUN = function(x) var(x$fs_visual_se),
      FUN.VALUE = numeric(1)
    )
    for (i in length(test_se)) {
      expect_identical(unname(test_se[i]), 0)
    }
  }
)

test_that(
  "Standard errors for each observation are positive numbers and within 1",
  {
    expect_gt(min(test_object_fs_multi[[1]]$fs_visual_se), 0)
    expect_lt(max(test_object_fs_multi[[1]]$fs_visual_se), 1)
    expect_gt(min(test_object_fs_multi[[2]]$fs_visual_se), 0)
    expect_lt(max(test_object_fs_multi[[2]]$fs_visual_se), 1)
  }
)

# Bartlett scores
test_object_fs_multi_bar <- get_fs(
  HolzingerSwineford1939[c("school", "x1", "x2", "x3")],
  hs_model,
  group = "school",
  method = "Bartlett"
)

test_that(
  "Standard errors for each observation are the same within groups",
  {
    test_se <- vapply(test_object_fs_multi_bar,
      FUN = function(x) var(x$fs_visual_se),
      FUN.VALUE = numeric(1)
    )
    for (i in length(test_se)) {
      expect_identical(unname(test_se[i]), 0)
    }
  }
)

test_that(
  "Standard errors for each observation are positive numbers and within 1",
  {
    expect_gt(min(test_object_fs_multi_bar[[1]]$fs_visual_se), 0)
    expect_lt(max(test_object_fs_multi_bar[[1]]$fs_visual_se), 1)
    expect_gt(min(test_object_fs_multi_bar[[2]]$fs_visual_se), 0)
    expect_lt(max(test_object_fs_multi_bar[[2]]$fs_visual_se), 1)
  }
)

###### Multiple factors example #####

# Prepare for test objects
hs_model_2 <- ' visual =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed =~ x7 + x8 + x9 '
test_object_fs_multi_2 <- get_fs(HolzingerSwineford1939,
                                 hs_model_2,
                                 group = "school")

########## Testing section ############

# Class of output

test_that("Number of factors is equal", {
  expected_cols <- ncol_cfa(
    cfa(model = hs_model_2, data = HolzingerSwineford1939)
  )
  expect_equal(length(test_object_fs_multi_2[[1]]), expected_cols + 1)
})

test_that("Number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs_multi_2[[1]]) +
                     nrow(test_object_fs_multi_2[[2]]),
                   nrow(HolzingerSwineford1939))
})

# Test standard errors

test_that(
  "Standard errors for each observation are the same within groups",
  {
    fs_names <- colnames(test_object_fs_multi_2[[1]])
    names_se <- grep("_se", fs_names, value = TRUE)
    for (i in names_se) {
      test_se <- vapply(test_object_fs_multi_2,
        FUN = function(x) var(x[[i]]),
        FUN.VALUE = numeric(1)
      )
      expect_identical(max(test_se), 0)
    }
  }
)

test_that(
  "Standard errors for each observation are positive numbers and within 1",
  {
    rb_test_object <- do.call(rbind, test_object_fs_multi_2)
    fs_names <- colnames(rb_test_object)
    names_se <- grep("_se", fs_names, value = TRUE)
    for (i in names_se) {
      expect_gt(min(rb_test_object[, i]), 0)
      expect_lt(max(rb_test_object[, i]), 1)
    }
  }
)

# Bartlett scores
test_object_fs_multi_2_bar <- get_fs(HolzingerSwineford1939,
                                     hs_model_2,
                                     group = "school")

test_that(
  "Standard errors for each observation are the same within groups",
  {
    rb_test_object <- do.call(rbind, test_object_fs_multi_2_bar)
    fs_names <- colnames(rb_test_object)
    names_se <- grep("_se", fs_names, value = TRUE)
    for (i in names_se) {
      test_se <- tapply(
        rb_test_object[[i]],
        rb_test_object$school, var
      )
      expect_identical(max(test_se), 0)
    }
  }
)

test_that(
  "Standard errors for each observation are positive numbers and within 1",
  {
    rb_test_object <- do.call(rbind, test_object_fs_multi_2_bar)
    fs_names <- colnames(rb_test_object)
    names_se <- grep("_se", fs_names, value = TRUE)
    for (i in names_se) {
      expect_gt(min(rb_test_object[, i]), 0)
      expect_lt(max(rb_test_object[, i]), 1)
    }
  }
)

fs_config <- get_fs(HolzingerSwineford1939,
                    hs_model_2,
                    group = "school",
                    corrected_fsT = TRUE
)

fs_metric <- get_fs(HolzingerSwineford1939,
                    hs_model_2,
                    group = "school",
                    group.equal = "loadings",
                    corrected_fsT = TRUE
)

fs_single <- get_fs(
  HolzingerSwineford1939 |>
    subset(school == "Grant-White"),
  hs_model_2,
  corrected_fsT = TRUE
)

test_that("Correction factor is similar with single or multiple groups", {
  fst1 <- attr(fs_config, "fsT")
  fst2 <- attr(fs_metric, "fsT")
  fst3 <- attr(fs_single, "fsT")
  expect_equal(fst1[[2]], fst3, tolerance = 1e-5)
  d1 <- fst1[[1]] - fst1[[2]]
  d2 <- fst2[[1]] - fst2[[2]]
  expect_lt(mean(abs(d2)), mean(abs(d1)))
})

########## Computing reliability ##########

test_that("Regression factor scores", {
  fs <- get_fs(PoliticalDemocracy[c("x1", "x2", "x3")],
               corrected_fsT = TRUE, reliability = TRUE)
  expect_equal(as.vector(attr(fs, "reliability")), .9790008,
               tolerance = 1e-7)
})

test_that("Bartlett factor scores", {
  fs <- get_fs(PoliticalDemocracy[c("x1", "x2", "x3")],
               corrected_fsT = TRUE, reliability = TRUE, method = "Bartlett")
  expect_equal(as.vector(attr(fs, "reliability")), .9790008,
               tolerance = 1e-7)
})



