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

### lavaan::standardizedSolution()
s2_std_beta_lav <- subset(standardizedSolution(fit1), op == "~")

test_that("Test for single group warning message",
{expect_message(grandStardardizedSolution(fit1), "The grand standardized solution is equivalent to the standardizedSolution() from lavaan for a model with a single group.", fixed=TRUE)})

test_that("Standardized beta in a model with single group, two factors",
          { expect_equal(s2_std_beta$est.std, s2_std_beta_lav$est.std) })
test_that("SE of standardized beta in a model with single group, two-factors",
          { expect_equal(s2_std_beta$se,  s2_std_beta_lav$se) })

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

### lavaan::standardizedSolution()
s3_std_beta_lav <- subset(standardizedSolution(fit2), op == "~")

apply(rbind(s3_std_beta_lav$est.std, s3_std_beta$est.std), 2,
      function(x) {
        test_that(
          "Standardized beta in a model with single group, three factors",
          { expect_lt(abs(diff(x)), err)}
        )
      })
apply(rbind(s3_std_beta_lav$se, s3_std_beta$se), 2,
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

## Hand calculation
### std.est
model_list <- lavTech(fit3, what = "est")
ns <- lavInspect(fit3, what = "nobs")
beta_list <- model_list[which(names(model_list) == "beta")]
psi_list <- model_list[which(names(model_list) == "psi")]
alpha_list <- model_list[which(names(model_list) == "alpha")]
v_eta <- veta_grand(ns,
                    beta_list,
                    psi_list = psi_list,
                    alpha_list = alpha_list)
s_eta <- sqrt(diag(v_eta))
inv_s_eta <- 1 / s_eta
std_betas <- lapply(beta_list, function(x) {
  diag(inv_s_eta) %*% x %*% diag(s_eta)
})
m2_std_betas_h <- unlist(std_betas)[unlist(std_betas) != 0]
### se
free_list <- lavTech(fit3, what = "free")
acov_par <- vcov(fit3)
free_beta_psi_alpha <- free_list[which(names(model_list) %in%
                                         c("beta", "psi", "alpha"))]
est <- .combine_est(model_list[which(names(model_list) %in%
                                       c("beta", "psi", "alpha"))],
                    free = free_beta_psi_alpha)
jac <- lav_func_jacobian_complex(function(x)
  unlist(grand_std_beta_est(model_list, ns = ns, free_list = free_list, est = x)),
  x = est)
pos_beta_psi_alpha <- .combine_est(free_beta_psi_alpha,
                                   free = free_beta_psi_alpha)
acov_beta_psi_alpha <- acov_par[pos_beta_psi_alpha, pos_beta_psi_alpha]
acov <- jac %*% acov_beta_psi_alpha %*% t(jac)
m2_std_se_h <- sqrt(acov[3, c(3, 7)])

apply(rbind(m2_std_betas_h, m2_std_beta$est.std), 2,
      function(x) {
        test_that(
          "Standardized beta in a model with multiple groups, two factors",
          { expect_lt(abs(diff(x)), err)}
        )
      })
apply(rbind(m2_std_se_h, m2_std_beta$se), 2,
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

## Hand calculation
### std.est
model_list <- lavTech(fit4, what = "est")
ns <- lavInspect(fit4, what = "nobs")
beta_list <- model_list[which(names(model_list) == "beta")]
psi_list <- model_list[which(names(model_list) == "psi")]
alpha_list <- model_list[which(names(model_list) == "alpha")]
v_eta <- veta_grand(ns,
                    beta_list,
                    psi_list = psi_list,
                    alpha_list = alpha_list)
s_eta <- sqrt(diag(v_eta))
inv_s_eta <- 1 / s_eta
std_betas <- lapply(beta_list, function(x) {
  diag(inv_s_eta) %*% x %*% diag(s_eta)
})
m3_std_betas_h <- unlist(std_betas)[unlist(std_betas) != 0]
### se
free_list <- lavTech(fit4, what = "free")
acov_par <- vcov(fit4)
free_beta_psi_alpha <- free_list[which(names(model_list) %in%
                                         c("beta", "psi", "alpha"))]
est <- .combine_est(model_list[which(names(model_list) %in%
                                       c("beta", "psi", "alpha"))],
                    free = free_beta_psi_alpha)
jac <- lav_func_jacobian_complex(function(x)
  unlist(grand_std_beta_est(model_list, ns = ns, free_list = free_list, est = x)),
  x = est)
pos_beta_psi_alpha <- .combine_est(free_beta_psi_alpha,
                                   free = free_beta_psi_alpha)
acov_beta_psi_alpha <- acov_par[pos_beta_psi_alpha, pos_beta_psi_alpha]
std_se <- jac %*% acov_beta_psi_alpha %*% t(jac)
m3_std_se_h <- sqrt(diag(std_se[c(4, 7, 13, 16), c(4, 7, 13, 16)]))

apply(rbind(m3_std_betas_h, m3_std_beta$est.std), 2,
      function(x) {
        test_that(
          "Standardized beta in a model with multiple groups, three factors",
          { expect_lt(abs(diff(x)), err)}
        )
      })
apply(rbind(m3_std_se_h, m3_std_beta$se), 2,
      function(x) {
        test_that(
          "SE of standardized beta in a model with multiple groups, three factors",
          { expect_lt(abs(diff(x)), err)}
        )
      })