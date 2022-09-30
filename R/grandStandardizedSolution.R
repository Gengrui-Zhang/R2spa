#' Grand Standardized Solution
#'
#' Grand standardized solution of a two-stage path analysis model.
#'
#' @param model_list A list of string variable describing the structural path
#'                   model, in \code{lavaan} syntax
#' @param se A Boolean variable. If TRUE, standard errors for the grand
#'                   standardized parameters will be computed.
#' @param acov_par An asymptotic variance-covariance matrix for a fitted
#'                 model object.
#' @param free_list A list of model matrices that indicate the position of
#'                  the free parameters in the parameter vector.
#' @return A matrix of the standardized model parameters and standard errors.
#' @export
#'
#' @example
#' library(lavaan)
#'
#' ## single group example
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
#'
#' ## multiple group example
#' reg <- '
#'   # latent variable definitions
#'     visual =~ x1 + x2 + x3
#'     speed =~ x7 + x8 + x9
#'   # regressions
#'     visual ~ c(b1, b1) * speed
#' '
#' reg_fit <- sem(reg, data = HolzingerSwineford1939,
#'                group = "school",
#'                group.equal = c("loadings", "intercepts"))
#'
#' grandStardardizedSolution(lavInspect(reg_fit, what = "est"),
#'                           ns = lavInspect(reg_fit, what = "nobs"),
#'                           se = TRUE, acov_par = vcov(reg_fit),
#'                           free_list = lavTech(reg_fit, what = "free"))


grandStardardizedSolution <- function(model_list,
                                      se = TRUE, acov_par = NULL,
                                      free_list = NULL) {
  tmp_std_beta <- std_beta_est(model_list)
  partable <- subset(inspect(fit, "list"), op == "~")
  out <- partable[, c("lhs", "op", "rhs", "exo", "group",
                       "block", "label")]
  out$est.std <- tmp_std_beta[which(tmp_std_beta != 0)]
  if (se) {
    free_beta_psi <- free_list[c("beta", "psi")]
    est <- .combine_est(model_list[c("beta", "psi")],
                        free = free_beta_psi)
    jac <- lav_func_jacobian_complex(function(x)
      std_beta_est(model_list, free_list = free_list, est = x),
      x = est)
    pos_beta_psi <- .combine_est(free_beta_psi, free = free_beta_psi)
    acov_beta_psi <- acov_par[pos_beta_psi, pos_beta_psi]
    tmp_acov_std_beta <- jac %*% acov_beta_psi %*% t(jac)
    out$se <- sqrt(tmp_acov_std_beta[which(tmp_acov_std_beta != 0)])
    out$z <- out$est.std / out$se
    out$pvalue <- 2 * (1 - pnorm(abs(out$z)))
    ci <- out$est.std + out$se * qnorm(c((1 - .95)/2, 1 - (1 - .95)/2))
    out$ci.lower <- ci[1]
    out$ci.upper <- ci[2]
  }
  class(out) <- c("lavaan.data.frame", "data.frame")
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
