library(semTools)
library(lavaan)
library(semPlot)
library(SimDesign)
library(MASS)
library(mnormt)
library(dplyr)
library(tidyverse)
library(MBESS)
devtools::load_all(".")

# Data Generation

# Helper Function
generate_sem_data <- function(N, model, Alpha, Phi, Lambda, Gamma, Theta, SD_y) {
  # Generate scores for observed items: x1 - x3, m1 - m3
  eta_scores <- rmnorm(N, mean = Alpha, varcov = Phi) # Factor scores
  # Generate eta scores with interaction
  eta_scores_int <- cbind(eta_scores, eta_scores[,1]*eta_scores[,2])
  delta <- rmnorm(N, mean = rep(0, length(diag(Theta))), varcov = Theta) # Errors/Residuals of Indicators
  item_scores <- tcrossprod(eta_scores, Lambda) + delta # Item/Indicator scores

  y_scores <- tcrossprod(eta_scores_int, Gamma) + rnorm(N, 0, SD_y) # Y scores / DV scores

  # Parsing formula
  indicator_terms <- unlist(strsplit(gsub(" ", "",
                                          unlist(strsplit(model, "\n"))[grep("=~",
                                                                             unlist(strsplit(model, "\n")))]),
                                     split = "=~"))
  indicator_vars <- indicator_terms[grepl("+", indicator_terms, fixed = TRUE) == "FALSE"]
  indicator_num <- as.vector(unlist(lapply(strsplit(indicator_terms[grepl("+", indicator_terms, fixed = TRUE) == "TRUE"],
                                                    split = "+", fixed = TRUE), length)))

  df <- as.data.frame(cbind(item_scores, y_scores))
  names <- list()
  for (n in seq(length(indicator_vars))) {
    names[[n]] <- tolower(paste0(indicator_vars[n], seq(indicator_num[n])))
  }
  colnames(df) <- c(unlist(names), "Y")
  return(df)
}

DESIGNFACTOR <- createDesign(
  N = c(100, 250, 500),
  beta1 = 1,  # fixed
  beta2 = 0.9,  # fixed
  beta3 = c(0.75, 0.80, 0.85),  # three conditions
  cor_xm = c(0, 0.3, 0.6), # correlation between latent x and m / error variance of Y
  rel = c(0.7, 0.8, 0.9)
)

FIXED_PARAMETER <- list(model = '
                                  # Measurement Model
                                    X =~ x1 + x2 + x3
                                    M =~ m1 + m2 + m3
                                  # Structural Model
                                    Y ~ b1*X + b2*M + b3*X:M
                                  # Define Standardized Coefficients
                                    X ~~ v1*X
                                    M ~~ v2*M
                                    beta1 := b1*sqrt(v1)
                                    beta2 := b2*sqrt(v2)
                                    beta3 := b3*sqrt(v1)*sqrt(v2)
                                  ',
                        mu_x = 0, # latent mean of x: fixed at 0
                        mu_m = 0, # latent mean of m: fixed at 0,
                        mu_xm = 0,
                        gamma = list(xy = 0.3, # linear effects of x and y: fixed at 0.3
                                     my = 0.3, # linear effects of x and y: fixed at 0.3
                                     xmy = 0.3) # # linear effects of xm and y: fixed at 0.3
)


GenData <- function (condition, fixed_objects = NULL) {
  N <- condition$N # Sample size
  beta1 <- condition$beta1 # beta 1: fixed at 1
  beta2 <- condition$beta2 # beta 2: fixed at 0.9
  beta3 <- condition$beta3 # beta 3: varied
  cor_xm <- condition$cor_xm # latent correlation: varied

  if (condition$cor_xm == 0) {
    sd_y <- sqrt(0.82)
  } else if (condition$cor_xm == 0.3) {
    sd_y <- sqrt(0.766)
  } else if (condition$cor_xm == 0.6) {
    sd_y <- sqrt(0.712)
  }

  if (condition$rel == 0.7) {
    rel_val <- c(1.32, 0.99, 0.69)
  } else if (condition$rel == 0.8) {
    rel_val <- c(0.77, 0.58, 0.40)
  } else if (condition$rel == 0.9) {
    rel_val <- c(0.34, 0.26, 0.18)
  }

  Alpha <- c(fixed_objects$mu_x, fixed_objects$mu_m) # Latent means
  Phi <- matrix(c(1, condition$cor_xm,
                  condition$cor_xm, 1), nrow = 2) # latent var/cov
  Lambda <- cbind(c(beta1, beta2, beta3, rep(0, 3)),
                  c(rep(0, 3), beta1, beta2, beta3)) # factor loadings
  Theta <- diag(rel_val,
                nrow = 6)
  Gamma <- rbind(unname(unlist(fixed_objects$gamma)))
  SD_y <- sd_y

  generate_sem_data(N,
                    model = fixed_objects$model,
                    Alpha = Alpha,
                    Phi = Phi,
                    Lambda = Lambda,
                    Theta = Theta,
                    Gamma = Gamma,
                    SD_y = SD_y
  )
}

extract_res <- function (condition, dat, fixed_objects = NULL) {

  # Fit using rapi function
  fit_rapi <- rapi(model = fixed_objects$model,
                       data = dat)
  # Fit using upi function
  fit_upi <- upi(model = fixed_objects$model,
                 data = dat,
                 mode = "all")
  # Fit using tspa function
  fs_dat <- get_fs(dat,
                   model ='
                             X =~ x1 + x2 + x3
                             M =~ m1 + m2 + m3
                             ',
                   method = "Bartlett",
                   std.lv = TRUE)
  Y <- dat$Y
  fs_dat <- cbind(fs_dat, Y)
  fit_tspa <- tspa(model = "Y ~ b1*X + b2*M + b3*X:M
                              beta1 := b1 * sqrt(v1)
                              beta2 := b2 * sqrt(v2)
                              beta3 := b3 * sqrt(v1) * sqrt(v2)",
                   data = fs_dat,
                   se = list(X = fs_dat$fs_X_se[1],
                             M = fs_dat$fs_M_se[1]))

  # Extract parameter estimates and standard errors
  paret <- c(coef(fit_rapi, type = "user")["beta3"],
             sqrt(vcov(fit_rapi, type = "user")["beta3", "beta3"]),
             coef(fit_upi, type = "user")["beta3"],
             sqrt(vcov(fit_upi, type = "user")["beta3", "beta3"]),
             coef(fit_tspa, type = "user")["beta3"],
             sqrt(vcov(fit_tspa, type = "user")["beta3", "beta3"]))
  names(paret) <- c("rapi_yint_est", "rapi_yint_se",
                    "upi_yint_est", "upi_yint_se",
                    "tspa_yint_est", "tspa_yint_se")
  return(paret)
}

evaluate_res <- function (condition, results, fixed_objects = NULL) {

  # Population parameter
  pop_par <- 0.3

  # Separate estimates and se
  results_est <- as.data.frame(results[colnames(results)[grepl("_est", colnames(results))]])
  results_se <- as.data.frame(results[colnames(results)[grepl("_se", colnames(results))]])

  # Helper function: robust bias
  robust_bias <- function (est_se, est, pop_par, trim = NULL, type = "trim") {
    if (type == "trim") {
      return(mean(est, trim = trim, na.rm = TRUE) - pop_par)
    } else if (type == "median"){
      return((median(est, na.rm = TRUE) - pop_par) / mad(est, na.rm = TRUE))
    } else {
      return((mean(est, trim = trim, na.rm = TRUE) - pop_par)/est_se)
    }
  }

  # Helper function: relative SE bias
  rse_bias <- function(est_se, est) {
    est_se <- as.matrix(est_se)
    est <- as.matrix(est)
    est_se <- colMeans(est_se)
    emp_sd <- apply(est, 2L, sd)
    est_se / emp_sd - 1
  }

  # Helper functuion for calculating coverage rate
  coverage_rate <- function(est_se, est, pop) {
    ci_est <- list()
    est_se <- as.matrix(est_se)
    est <- as.matrix(est)
    lo.95 <- est - qnorm(.975)*est_se
    hi.95 <- est + qnorm(.975)*est_se
    for (i in seq_len(length(colnames(est)))) {
      ci_est[[i]] <- cbind(lo.95[,i], hi.95[,i])
      names(ci_est)[i] <- colnames(est)[i]
    }
    return(unlist(lapply(ci_est, ECR, parameter = pop)))
  }

  c(raw_bias = bias(results_est,
                    parameter = pop_par),
    std_bias = bias(results_est,
                    parameter = pop_par,
                    type = "standardized"),
    trimm_bias = robust_bias(results_se,
                              results_est,
                              pop_par,
                              trim = 0.1,
                              type = "trim"), # 10% trimmed mean
    stdMed_bias = robust_bias(results_se,
                              results_est,
                              pop_par,
                              type = "median"),
    coverage = coverage_rate(results_se,
                             results_est,
                             pop = pop_par),
    rmse = RMSE(results_est,
                parameter = pop_par),
    rse_bias = rse_bias(results_se,
                        results_est)
  )
}

# Run 2000 replications

 All_BoundBeta_02062024 <- runSimulation(design = DESIGNFACTOR,
                                          replications = 2000,
                                          generate = GenData,
                                          analyse = extract_res,
                                          summarise = evaluate_res,
                                          fixed_objects = FIXED_PARAMETER,
                                          save = TRUE,
                                          save_results = TRUE,
                                          filename = "All_BoundBeta_02062024",
                                          parallel = TRUE,
                                          ncores = min(4L, parallel::detectCores() - 1))
