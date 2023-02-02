# Replication of Hsiao (2021)

library(semTools)
library(lavaan)
library(semPlot)
library(SimDesign)
library(MASS)
library(mnormt)
library(dplyr)
library(tidyverse)
library(ufs)
library(MBESS)

# Data Generation

 # Helper Function
 generate_sem_data <- function(N, model, Alpha, Phi, Lambda, Gamma, Theta, SD_y) {
   # Generate scores for observed items: x1 - x3, m1 - m3
   eta_scores <- rmnorm(N, mean = Alpha, varcov = Phi) # Factor scores
   # Generate eta scores with interaction
   eta_scores_int <- cbind(eta_scores, eta_scores[,1]*eta_scores[,2])
   delta <- rmnorm(N, varcov = Theta) # Errors/Residuals of Indicators
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
   N = c(250, 500),
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
                                    Y ~ X + M + X:M
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
   Theta<- diag(rel_val,
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
    fit_rapi <- rapi(model = fixed_objects$model, data = dat)
  # Fit using upi function
    fit_upi <- upi(model = fixed_objects$model, data = dat)
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
    fit_tspa <- tspa(model = "Y ~ X + M + X:M",
                     data = fs_dat,
                     se = list(X = fs_dat$fs_X_se[1],
                               M = fs_dat$fs_M_se[1]))

  # Extract parameter estimates and standard errors
    paret <- c(coef(fit_rapi)["Y~X"],
               sqrt(vcov(fit_rapi)["Y~X", "Y~X"]),
               coef(fit_upi)["Y~X"],
               sqrt(vcov(fit_upi)["Y~X", "Y~X"]),
               coef(fit_tspa)["Y~X"],
               sqrt(vcov(fit_tspa)["Y~X", "Y~X"]),
               coef(fit_rapi)["Y~M"],
               sqrt(vcov(fit_rapi)["Y~M", "Y~M"]),
               coef(fit_upi)["Y~M"],
               sqrt(vcov(fit_upi)["Y~M", "Y~M"]),
               coef(fit_tspa)["Y~M"],
               sqrt(vcov(fit_tspa)["Y~M", "Y~M"]),
               coef(fit_rapi)["Y~int"],
               sqrt(vcov(fit_rapi)["Y~int", "Y~int"]),
               coef(fit_upi)["Y~X_int_M"],
               sqrt(vcov(fit_upi)["Y~X_int_M", "Y~X_int_M"]),
               coef(fit_tspa)["Y~X.M"],
               sqrt(vcov(fit_tspa)["Y~X.M", "Y~X.M"]))
    names(paret) <- c("rapi_yx_est", "rapi_yx_se", "upi_yx_est", "upi_yx_se", "tspa_yx_est", "tspa_yx_se",
                      "rapi_ym_est", "rapi_ym_se", "upi_ym_est", "upi_ym_se", "tspa_ym_est", "tspa_ym_se",
                      "rapi_yint_est", "rapi_yint_se", "upi_yint_est", "upi_yint_se", "tspa_yint_est", "tspa_yint_se")
    return(paret)
 }

#--------------------------------------------------------------------------------------------------------------------
 # Test it
 test_df <- GenData(condition = DESIGNFACTOR[22, ], fixed_objects = FIXED_PARAMETER)
 test_results <- extract_res(condition = DESIGNFACTOR[22, ], dat = test_df, fixed_objects = FIXED_PARAMETER)
 # Test alpha
 test_rel_x <- test_df %>%
                   select(x1:x3) %>%
                   scaleStructure()
 test_rel_x$output$dat
#--------------------------------------------------------------------------------------------------------------------

 evaluate_res <- function (condition, results, fixed_objects = NULL) {
   # Helper function: relative SE bias
   rse_bias <- function(est_se, est) {
     est_se <- as.matrix(est_se)
     est <- as.matrix(est)
     est_se <- colMeans(est_se)
     emp_sd <- apply(est, 2L, sd)
     est_se / emp_sd - 1
   }

   gamma <- 0.3

   c(
     bias = bias(results[, c("rapi_yx_est", "upi_yx_est", "tspa_yx_est",
                             "rapi_ym_est", "upi_ym_est", "tspa_ym_est",
                             "rapi_yint_est", "upi_yint_est", "tspa_yint_est")],
                 parameter = gamma),
     std_bias = bias(results[ , c("rapi_yx_est", "upi_yx_est", "tspa_yx_est",
                                  "rapi_ym_est", "upi_ym_est", "tspa_ym_est",
                                  "rapi_yint_est", "upi_yint_est", "tspa_yint_est")],
                     parameter = gamma,
                     type = "standardized"),
     rmse = RMSE(results[ , c("rapi_yx_est", "upi_yx_est", "tspa_yx_est",
                              "rapi_ym_est", "upi_ym_est", "tspa_ym_est",
                              "rapi_yint_est", "upi_yint_est", "tspa_yint_est")],
                 parameter = gamma),
     rse_bias = rse_bias(results[ , c("rapi_yx_se", "upi_yx_se", "tspa_yx_se",
                                      "rapi_ym_se", "upi_ym_se", "tspa_ym_se",
                                      "rapi_yint_se", "upi_yint_se", "tspa_yint_se")],
                         results[ , c("rapi_yx_est", "upi_yx_est", "tspa_yx_est",
                                      "rapi_ym_est", "upi_ym_est", "tspa_ym_est",
                                      "rapi_yint_est", "upi_yint_est", "tspa_yint_est")])
   )
 }

# Run 200 replications

 sim_trial <- runSimulation(design = DESIGNFACTOR,
                            replications = 200,
                            generate = GenData,
                            analyse = extract_res,
                            summarise = evaluate_res,
                            fixed_objects = FIXED_PARAMETER,
                            save = TRUE,
                            save_results = TRUE,
                            filename = "simulation_result_1",
                            parallel = TRUE,
                            ncores = min(4L, parallel::detectCores() - 1))

# Summarize the results

 sim_results <- sim_trial %>%
   gather("var", "val", bias.rapi_yx_est:rse_bias.tspa_yint_se) %>%
   select(-c(SIM_TIME:WARNINGS)) %>%
   separate(col = var, into = c("stats", "parmet"), sep = "\\.") %>%
   separate(col = parmet, into = c("method", "par", "result"),  sep = "_") %>%
   select(-result) %>%
   spread(stats, val) %>%
   relocate(REPLICATIONS, .after = last_col()) %>%
   mutate(N_lab = as_factor(paste0("italic(N) == ", N)),
          beta1_lab = as_factor(paste0("\\beta_{1} == ", beta1)),
          beta2_lab = as_factor(paste0("\\beta_{1} == ", beta2)),
          beta3_lab = as_factor(paste0("\\beta_{1} == ", beta3)),
          cor_xm_lab = as_factor(paste0("Correlation_XM == ", cor_xm)),
          rel_lab = as_factor(paste0("Reliability == ", rel)))

 write_csv(sim_results, "sim_results_1218.csv")

# Plot the results

 sim_results <- read.csv("sim_results_1218.csv")
 # Bias
 sim_results %>%
   ggplot(aes(x = factor(N), y = bias, color = method)) +
   geom_boxplot() +
   facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
   labs(x = "Sample Size (N)", y = "Bias")

 # Standard Bias
 sim_results %>%
   ggplot(aes(x = factor(N), y = std_bias, color = method)) +
   geom_boxplot() +
   facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
   labs(x = "Sample Size (N)", y = "Standard Bias")

 # Relative SE Bias
 sim_results %>%
   ggplot(aes(x = factor(N), y = rse_bias, color = method)) +
   geom_boxplot() +
   facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
   labs(x = "Sample Size (N)", y = "Relative SE Bias")

 # RMSE
 sim_results %>%
   ggplot(aes(x = factor(N), y = rmse, color = method)) +
   geom_boxplot() +
   facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
   labs(x = "Sample Size (N)", y = "RMSE")

#--------------------------------------------------------------------------------------------------------------------
 # Compare with Hsiao 2021
 DESIGNFACTOR <- createDesign(
   N = c(250, 500),
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
                                    Y ~ X + M + X:M
                                  ',
                         mu_x = 0, # latent mean of x: fixed at 0
                         mu_m = 0, # latent mean of m: fixed at 0,
                         mu_xm = 0,
                         gamma = list(xy = 0.3, # linear effects of x and y: fixed at 0.3
                                      my = 0.3, # linear effects of x and y: fixed at 0.3
                                      xmy = 0.3), # # linear effects of xm and y: fixed at 0.3
                         pop_loading = list(large = 0.256,
                                            medium = 0.247,
                                            small = 0.238)
 )

 extract_res <- function (condition, dat, fixed_objects = NULL) {
   # rapi with alpha
   rapi_alpha <- rapi_rel(model = fixed_objects$model,
                          data = dat,
                          rel = "alpha")
   # rapi with omega
   rapi_omega <- rapi_rel(model = fixed_objects$model,
                          data = dat,
                          rel = "omega")
   # rapi with H
   rapi_H <- rapi_rel(model = fixed_objects$model,
                      data = dat,
                      rel = "H")
   # rapi with GLB
   rapi_GLB <- rapi_rel(model = fixed_objects$model,
                        data = dat,
                        rel = "GLB")
   # Extract parameter estimates and standard errors
   paret <- c(coef(rapi_alpha)["Y~int"],
              sqrt(vcov(rapi_alpha)["Y~int", "Y~int"]),
              coef(rapi_omega)["Y~int"],
              sqrt(vcov(rapi_omega)["Y~int", "Y~int"]),
              coef(rapi_H)["Y~int"],
              sqrt(vcov(rapi_H)["Y~int", "Y~int"]),
              coef(rapi_GLB)["Y~int"],
              sqrt(vcov(rapi_GLB)["Y~int", "Y~int"]))
   names(paret) <- c("rapi_alpha_est", "rapi_alpha_se",
                     "rapi_omega_est", "rapi_omega_se",
                     "rapi_H_est", "rapi_H_se",
                     "rapi_GLB_est", "rapi_GLB_se")
   return(paret)
 }

 evaluate_res <- function (condition, results, fixed_objects = NULL) {

   # Population parameter
   pop_par <- ifelse(condition$beta3 == 0.75, fixed_objects$pop_loading$large,
                     ifelse(condition$beta3 == 0.8, fixed_objects$pop_loading$medium,
                            ifelse(condition$beta3 == 0.85, fixed_objects$pop_loading$small, NA)))


   # Separate estimates and se
   results_est <- as.data.frame(results[colnames(results)[grepl("_est", colnames(results))]])
   results_se <- as.data.frame(results[colnames(results)[grepl("_se", colnames(results))]])

   # Helper function for calculating relative se bias
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

   browser()

   c(
     std_bias = bias(results_est,
                     parameter = pop_par,
                     type = "standardized"),
     coverage = coverage_rate(results_se,
                              results_est,
                              pop = pop_par),
     rmse = RMSE(results_est,
                 parameter = pop_par),
     rse_bias = rse_bias(results_se,
                         results_est)
   )
 }

 sim_hsiao <- runSimulation(design = DESIGNFACTOR,
                            replications = 2000,
                            generate = GenData,
                            analyse = extract_res,
                            summarise = evaluate_res,
                            fixed_objects = FIXED_PARAMETER,
                            # save = TRUE,
                            # save_results = TRUE,
                            # filename = "hsiao_result",
                            # parallel = TRUE,
                            # ncores = min(4L, parallel::detectCores() - 1)
 )




