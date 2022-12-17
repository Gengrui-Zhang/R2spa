library(lavaan)
# - [ ] add unit tests for std_beta_est()

# Test grandStardardizedSolution -----------------------------------------------

## Maximum expected error of estimates
err <- .0001

## Single-group, two-factor

mod1 <- '
   # latent variables
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
   # regressions
     dem60 ~ ind60
'
fit1 <- sem(model = mod1,
            data  = PoliticalDemocracy)
s2_std_beta <- grandStardardizedSolution(fit1)

test_that("Test for single group warning message",
{expect_message(grandStardardizedSolution(fit1), "The grand standardized solution is equivalent to the standardizedSolution() from lavaan for a model with a single group.", fixed=TRUE)})

test_that("Standardized beta in a model with single group, two factors",
          { expect_lt(abs(0.4604657 - s2_std_beta$est.std), .0001) })
test_that("SE of standardized beta in a model with single group, two-factors",
          { expect_lt(abs(0.1002537 - s2_std_beta$se), .0001) })

## Single-group, three-factor

mod2 <- '
    # latent variables
      ind60 =~ x1 + x2 + x3
      dem60 =~ y1 + y2 + y3 + y4
      dem65 =~ y5 + y6 + y7 + y8
    # regressions
      dem60 ~ ind60
      dem65 ~ ind60 + dem60
'
fit2 <- sem(model = mod2,
            data  = PoliticalDemocracy)
s3_std_beta <- grandStardardizedSolution(fit2)

apply(rbind(c(0.4482202, 0.1455739, 0.9128180), s3_std_beta$est.std), 2,
      function(x) {
        test_that(
          "Standardized beta in a model with single group, three factors",
          { expect_lt(abs(diff(x)), err)}
        )
      })
apply(rbind(c(0.10202467, 0.07029265, 0.04774052), s3_std_beta$se), 2,
      function(x) {
        test_that(
          "SE of standardized beta in a model with single group, three factors",
          { expect_lt(abs(diff(x)), err)}
        )
      })

# Multigroup, two-factor

mod3 <- '
  # latent variable definitions
    visual =~ x1 + x2 + x3
    speed =~ x7 + x8 + x9
  # regressions
    visual ~ c(b1, b1) * speed
'
fit3 <- sem(mod3, data = HolzingerSwineford1939,
            group = "school",
            group.equal = c("loadings", "intercepts"))

m2_std_beta <- grandStardardizedSolution(fit3)

apply(rbind(c(0.4308889, 0.4308889), m2_std_beta$est.std), 2,
      function(x) {
        test_that(
          "Standardized beta in a model with multiple groups, two factors",
          { expect_lt(abs(diff(x)), err)}
        )
      })
apply(rbind(c(0.07344303, 0.07344303), m2_std_beta$se), 2,
      function(x) {
        test_that(
          "SE of standardized beta in a model with multiple groups, two factors",
          { expect_lt(abs(diff(x)), err)}
        )
      })

# Multigroup, three-factor

mod4 <- '
  # latent variable definitions
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9

  # regressions
    visual ~ c(b1, b1) * textual + c(b2, b2) * speed
'
fit4 <- sem(mod4, data = HolzingerSwineford1939,
            group = "school",
            group.equal = c("loadings", "intercepts"))
m3_std_beta <- grandStardardizedSolution(fit4)

apply(rbind(c(0.4187841, 0.3239380, 0.4187841, 0.3239380), m3_std_beta$est.std), 2,
      function(x) {
        test_that(
          "Standardized beta in a model with multiple groups, three factors",
          { expect_lt(abs(diff(x)), err)}
        )
      })
apply(rbind(c(0.07341362, 0.07814756, 0.07341362, 0.07814756), m3_std_beta$se), 2,
      function(x) {
        test_that(
          "SE of standardized beta in a model with multiple groups, three factors",
          { expect_lt(abs(diff(x)), err)}
        )
      })
