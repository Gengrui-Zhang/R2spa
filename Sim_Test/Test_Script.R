library(semTools)
library(lavaan)
library(semPlot)
library(SimDesign)
library(MASS)
library(mnormt)
library(dplyr)
library(tidyverse)
library(MBESS)
source("/Users/jimmy_z/R Projects/R2spa/Sim_Test/upi_test.R")
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

  # Fit using upi function
  fit_upi <- upi_test(model = fixed_objects$model,
                     data = dat,
                     mode = "all")
  if (lavInspect(fit_upi, what = "converged")) {
    upi_est <- coef(fit_upi, type = "user")["beta3"]
    upi_se <- sqrt(vcov(fit_upi, type = "user")["beta3", "beta3"])
  } else {
    upi_est <- NA
    upi_se <- NA
  }
  # Extract parameter estimates and standard errors
  paret <- c(upi_est, upi_se)
  names(paret) <- c("upi_yint_est", "upi_yint_se")
  return(paret)
}

evaluate_res <- function (condition, results, fixed_objects = NULL) {

  # Population parameter
  pop_par <- 0.3

  # Separate estimates and se
  results_est <- as.data.frame(results[colnames(results)[grepl("_est", colnames(results))]])
  results_se <- as.data.frame(results[colnames(results)[grepl("_se", colnames(results))]])

  # Helper function: robust bias
  robust_bias <- function(est, se, pop_par, trim = 0, type = NULL) {
    output <- numeric(ncol(est))
    for (i in seq_len(ncol(est))) {
      if (type == "raw") {
        output[i] <- mean((est[,i] - pop_par), na.rm = TRUE)
      } else if (type == "standardized") {
        output[i] <- (mean(est[,i], na.rm = TRUE) - pop_par)/sd(est[,i], na.rm = TRUE)
      } else if (type == "trim") {
        output[i] <- mean(est[,i], trim = trim, na.rm = TRUE) - pop_par
      } else if (type == "median") {
        output[i] <- (median(est[,i], na.rm = TRUE) - pop_par) / mad(est[,i], na.rm = TRUE)
      } else {
        output[i] <- (mean(est[,i], trim = trim, na.rm = TRUE) - pop_par) / sd(est[,i], na.rm = TRUE)
      }
    }
    names(output) <- colnames(est)
    return(output)
  }

  # Helper function: relative SE bias
  rse_bias <- function(est, est_se, trim = 0, type = "raw") {
    if (type == "raw") {
      est_se <- as.matrix(est_se)
      est <- as.matrix(est)
      est_se_mean <- apply(est_se, 2, mean, na.rm = T)
      emp_sd <- apply(est, 2L, sd, na.rm = T)
      rse_bias <- est_se_mean / emp_sd - 1
    } else if (type == "median") {
      est_se <- as.matrix(est_se)
      est <- as.matrix(est)
      est_se_median <- apply(est_se, 2, median, na.rm = TRUE)
      emp_mad <- apply(est, 2, function(x) mad(x, na.rm = TRUE))
      rse_bias <- est_se_median / emp_mad - 1
    } else if (type == "trim") {
      est_se <- as.matrix(est_se)
      est <- as.matrix(est)
      est_se_mean <- apply(est_se, 2, mean, trim = trim, na.rm = TRUE)
      emp_sd <- apply(est, 2L, sd, na.rm = T)
      rse_bias <- est_se_mean / emp_sd - 1
    }
    return(rse_bias)
  }

  # Helper functuion for calculating coverage rate
  coverage_rate <- function(est, est_se, pop) {
    ci_est <- list()
    est_se <- as.matrix(est_se)
    est <- as.matrix(est)
    lo.95 <- est - qnorm(.975)*est_se
    hi.95 <- est + qnorm(.975)*est_se
    for (i in seq_len(length(colnames(est)))) {
      ci_est[[i]] <- cbind(lo.95[,i], hi.95[,i])
      names(ci_est)[i] <- colnames(est)[i]
    }
    ci_est <- lapply(ci_est, na.omit)
    return(unlist(lapply(ci_est, ECR, parameter = pop)))
  }

  # Helper function for convergence rate
  convergence_rate <- function(est) {
    apply(est, 2, function(x) 1-(sum(is.na(x)) / length(x)))
  }

  c(raw_bias = robust_bias(results_est,
                           results_se,
                           pop_par,
                           type = "raw"),
    std_bias = robust_bias(results_est,
                           results_se,
                           pop_par,
                           type = "standardized"),
    trim_bias = robust_bias(results_est,
                            results_se,
                            pop_par,
                            trim = 0.2,
                            type = "trim"), # 20% trimmed mean
    stdMed_bias = robust_bias(results_est,
                              results_se,
                              pop_par,
                              type = "median"),
    coverage = coverage_rate(results_est,
                             results_se,
                             pop = pop_par),
    rmse = RMSE(na.omit(results_est),
                parameter = pop_par),
    raw_rse_bias = rse_bias(results_est,
                            results_se,
                            type = "raw"),
    stdMed_rse_bias = rse_bias(results_est,
                               results_se,
                               type = "median"),
    trim_rse_bias = rse_bias(results_est,
                             results_se,
                             trim = 0.2,
                             type = "trim"),
    convergence_rate = convergence_rate(results_est)
  )
}

# Run 2000 replications

upi_test <- runSimulation(design = DESIGNFACTOR,
                               replications = 100,
                               generate = GenData,
                               analyse = extract_res,
                               summarise = evaluate_res,
                               fixed_objects = FIXED_PARAMETER,
                               save = TRUE,
                               save_results = TRUE,
                               filename = "upi_test",
                               control = list(allow_na = TRUE),
                               parallel = TRUE,
                               ncores = min(4L, parallel::detectCores() - 1))
