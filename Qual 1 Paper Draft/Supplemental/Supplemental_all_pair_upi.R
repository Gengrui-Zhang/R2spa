seed_value <- 14589
set.seed(seed_value)

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
  beta = c("parallel", "con_fac", "con_err"),
  cor_xm = c(0, 0.3, 0.6), # correlation between latent x and m / error variance of Y
  rel = 0.9
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
  cor_xm <- condition$cor_xm # latent correlation: varied
  beta <- rep(NA, 3)

  # Error variances
  if (condition$beta == "parallel") {
    beta <- rep(0.7, 3)
    sum_error <- sum(beta)^2*(1-0.9)/0.9
    rel_val <- rep(sum_error/3, 3)
  } else if (condition$beta == "con_fac") {
    beta <- c(1, 0.9, 0.75)
    sum_error <- sum(beta)^2*(1-0.9)/0.9
    rel_val <- rep(sum_error/3, 3)
  } else if (condition$beta == "con_err") {
    beta <- c(1, 0.9, 0.75)
    sum_error <- sum(beta)^2*(1-0.9)/0.9
    rel_val <- sum_error*c(0.44, 0.33, 0.23)
  }

  sd_y <- sqrt(1 - (fixed_objects$gamma[[1]]^2*1 + fixed_objects$gamma[[2]]^2*1 +
                        2*fixed_objects$gamma[[1]]*fixed_objects$gamma[[2]]*cor_xm +
                        fixed_objects$gamma[[3]]^2*(1 + cor_xm ^2)))

  Alpha <- c(fixed_objects$mu_x, fixed_objects$mu_m) # Latent means
  Phi <- matrix(c(1, condition$cor_xm,
                  condition$cor_xm, 1), nrow = 2) # latent var/cov
  Lambda <- cbind(c(beta, rep(0, 3)),
                  c(rep(0, 3), beta)) # factor loadings
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
  fit_upi <- upi(model = fixed_objects$model,
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

  # Descriptive of SEs
  descriptive <- function(est, se, type = NULL) {
    output <- numeric(ncol(est))
    if (type == "SD") {
      output <- apply(est, 2L, sd, na.rm = T)
    } else if (type == "MeanSE") {
      output <- apply(se, 2, mean, na.rm = T)
    } else if (type == "MedianSE") {
      output <- apply(se, 2, median, na.rm = TRUE)
    } else if (type == "MAD") {
      output <- apply(est, 2, function(x) mad(x, na.rm = TRUE))
    }
    return(output)
  }

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

  # Helper function: detecting outliers for SE
  outlier_se <- function(est_se) {
    results <- c()
    for(column in names(est_se)) {
      # Calculate Q1, Q3, and IQR
      Q1 <- quantile(est_se[[column]], 0.25, na.rm = TRUE)
      Q3 <- quantile(est_se[[column]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      # Determine outliers
      lower_bound <- (Q1 - 1.5 * IQR)
      upper_bound <- (Q3 + 1.5 * IQR)
      outliers <- est_se[[column]][est_se[[column]] < lower_bound | est_se[[column]] > upper_bound]
      # Calculate the percentage of outliers
      percentage <- length(outliers) / sum(!is.na(est_se[[column]])) * 100
      results[column] <- percentage
    }
    return(results)
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
    SD = descriptive(results_est,
                     results_se,
                     type = "SD"),
    MeanSE = descriptive(results_est,
                         results_se,
                         type = "MeanSE"),
    MedianSE = descriptive(results_est,
                           results_se,
                           type = "MedianSE"),
    MAD = descriptive(results_est,
                      results_se,
                      type = "MAD"),
    trim_rse_bias = rse_bias(results_est,
                             results_se,
                             trim = 0.2,
                             type = "trim"),
    outlier_se = outlier_se(results_se),
    convergence_rate = convergence_rate(results_est)
  )
}

# Run 2000 replications

All_02262024 <- runSimulation(design = DESIGNFACTOR,
                              replications = 2000,
                              generate = GenData,
                              analyse = extract_res,
                              summarise = evaluate_res,
                              fixed_objects = FIXED_PARAMETER,
                              save = TRUE,
                              save_results = TRUE,
                              filename = "All_02262024",
                              control = list(allow_na = TRUE),
                              parallel = TRUE,
                              ncores = min(4L, parallel::detectCores() - 1))

All_02262024$seed_value <- seed_value
