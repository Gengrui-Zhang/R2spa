#' Grand Standardized Solution
#'
#' @param model_list A list of string variable describing the structural path
#'                   model, in \code{lavaan} syntax
#' @param se A Boolean variable indicating .
#' @param acov_par
#' @param free_list
#'
#' @return
#'
#' @example
#' library(lavaan)
#'
#' myModel <- '
#'    # latent variables
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + y2 + y3 + y4
#'    # regressions
#'      dem60 ~ ind60
#' '
#' fit <- sem(model = myModel,
#'           data  = PoliticalDemocracy)
#'
#' lavInspect(fit, what = "cov.lv")  # lavaan
#' veta(fit@Model@GLIST$beta, fit@Model@GLIST$psi)
#'
#'
#' std_beta_est(lavInspect(fit, what = "est"),
#'              free_list = lavInspect(fit, what = "free"),
#'              est = c(1.5, 0.449, 3.453))
#'
#' grandStardardizedSolution(lavInspect(fit, what = "est"), se = TRUE,
#'                           acov_par = vcov(fit),
#'                           free_list = lavInspect(fit, what = "free"))

grandStardardizedSolution <- function(model_list, se = TRUE, acov_par = NULL,
                                      free_list = NULL) {
  out <- list(std_beta = std_beta_est(model_list))
  if (se) {
    free_beta_psi <- free_list[c("beta", "psi")]
    est <- .combine_est(model_list[c("beta", "psi")],
                        free = free_beta_psi)
    jac <- lav_func_jacobian_complex(function(x)
      std_beta_est(model_list, free_list = free_list, est = x),
      x = est)
    pos_beta_psi <- .combine_est(free_beta_psi, free = free_beta_psi)
    acov_beta_psi <- acov_par[pos_beta_psi, pos_beta_psi]
    out$acov_std_beta <- jac %*% acov_beta_psi %*% t(jac)
  }
  out
}

# Latent variances
veta <- function(beta, psi, gamma = NULL, cov_x = NULL) {
  inv_Imb <- solve(diag(nrow = nrow(beta)) - beta)
  if (!is.null(gamma) && !is.null(cov_x)) {
    psi_plus_vgammax <- psi + gamma %*% cov_x %*% t(gamma)
  } else {
    psi_plus_vgammax <- psi
  }
  inv_Imb %*% psi_plus_vgammax %*% t(inv_Imb)
}

.fill_matrix_list <- function(mod, free, est) {
  start_idx <- 0
  for (m in seq_along(mod)) {
    len_m <- sum(free[[m]] != 0)
    if (len_m == 0) next
    m_idx <- start_idx + seq_len(len_m)
    mod[[m]][free[[m]] != 0] <- est[m_idx]
    start_idx <- tail(m_idx, n = 1)
  }
  mod
}

std_beta_est <- function(model_list, free_list = NULL, est = NULL) {
  # The `est` argument is used to evaluate how changes in parameters
  # affect the standardized estimates, and is used to obtain the
  # derivatives for the delta method.
  if (!is.null(est) && !is.null(free_list)) {
    model_list <- .fill_matrix_list(model_list[c("beta", "psi")],
                                    free = free_list[c("beta", "psi")],
                                    est = est)
  }
  beta <- model_list$beta
  psi <- model_list$psi
  v_eta <- veta(beta, psi = psi)
  s_eta <- sqrt(diag(v_eta))
  inv_s_eta <- 1 / s_eta
  diag(inv_s_eta) %*% beta %*% diag(s_eta)
}

# Function for combining free estimates into a vector
.combine_est <- function(mod, free) {
  out <- vector("list", length(free))
  for (m in seq_along(free)) {
    out[[m]] <- mod[[m]][free[[m]] != 0]
  }
  unlist(out)
}
