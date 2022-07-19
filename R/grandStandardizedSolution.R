
#' @example
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
