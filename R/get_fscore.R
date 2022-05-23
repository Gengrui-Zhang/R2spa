#' Get Factor Scores and the Corresponding Reliability
#' @param data A data frame containing indicators.
#' @param model An optional string specifying the measurement model
#'              in \code{lavaan} syntax.
#'              See \code{\link[lavaan]{model.syntax}} for more information.
#' @param group Character. Name of the grouping variable for multiple group
#'              analysis, which is passed to \code{\link[lavaan]{cfa}}.
#' @param ... additional arguments passed to \code{\link[lavaan]{cfa}}. See
#'            \code{\link[lavaan]{lavOptions}} for a complete list.
#' @return A data frame containing the factor scores (prefaced with "fs_"),
#'         the standard errors, and the corresponding reliability.
#' @examples
#' library(lavaan)
#' get_fs(PoliticalDemocracy[c("x1", "x2", "x3")])
#'
#' # Multiple factors
#' get_fs(PoliticalDemocracy[c("x1", "x2", "x3", "y1", "y2", "y3", "y4")],
#'        model = " ind60 =~ x1 + x2 + x3
#'                  dem60 =~ y1 + y2 + y3 + y4 ")
#'
#' # Multiple-group
#' hs_model <- ' visual  =~ x1 + x2 + x3 '
#' fit <- cfa(hs_model,
#'            data = HolzingerSwineford1939,
#'            group = "school")
#' get_fs(HolzingerSwineford1939, hs_model, group = "school")
#' # Or without the model
#' get_fs(HolzingerSwineford1939[c("school", "x4", "x5", "x6")],
#'        group = "school")

get_fs <- function(data, model = NULL, group = NULL, ...) {
  if (!is.data.frame(data)) data <- as.data.frame(data)
  if (is.null(model)) {
    ind_names <- colnames(data)
    if (!is.null(group)) {
      ind_names <- setdiff(ind_names, group)
    }
    model <- paste("f1 =~",
                   paste(ind_names, collapse = " + "))
  }
  fit <- cfa(model, data = data, group = group, ...)
  est <- lavInspect(fit, what = "est")
  fscore <- lavPredict(fit, se = "standard")
  fscore_se <- attr(fscore, "se")
  if (is.null(group)) {
    augment_fs(est, fscore, fscore_se[[1]])
  } else {
    fs_list <- lapply(seq_along(est), function(i) {
      fs_dat <- augment_fs(est[[i]], fscore[[i]], fscore_se[[i]])
      fs_dat[[group]] <- names(est[i])
      fs_dat
    })
    do.call(rbind, fs_list)
  }
}

augment_fs <- function(est, fs, fs_se) {
  psi <- est$psi
  fs_rho <- 1 - fs_se^2 / diag(psi)
  colnames(fs) <- paste0("fs_", colnames(fs))
  colnames(fs_se) <- paste0("fs_", colnames(fs_se), "_se")
  colnames(fs_rho) <- paste0("fs_", colnames(fs_rho), "_rel")
  cbind(as.data.frame(fs), fs_se, fs_rho)
}

#' Compute factor scores
#'
#' @param y An N' x p matrix where each row is a response vector. If there
#'          is only one observation, it should be a matrix of one row.
#' @param lambda A p x q matrix of factor loadings.
#' @param theta A p x p matrix of unique variance-covariances.
#' @param psi A q x q matrix of latent factor variance-covariances.
#' @param nu A vector of length p of measurement intercepts.
#' @param alpha A vector of length q of latent means.
#' @param method A character string indicating the method for computing factor
#'               scores. Currently, only "regression" is supported.
#'
#' @return An N' x p matrix of factor scores.
#' @export
#'
#' @examples
#' library(lavaan)
#' fit <- cfa(" ind60 =~ x1 + x2 + x3
#'              dem60 =~ y1 + y2 + y3 + y4 ",
#'            data = PoliticalDemocracy)
#' fs_lavaan <- lavPredict(fit, method = "regression")
#' # Using R2spa::fscore()
#' est <- lavInspect(fit, what = "est")
#' fs_hand <- fscore(lavInspect(fit, what = "data"),
#'                   lambda = est$lambda,
#'                   theta = est$theta,
#'                   psi = est$psi)
#' fs_hand - fs_lavaan  # same scores
fscore <- function(y, lambda, theta, psi,
                   nu = colMeans(y), alpha = rep(0, nrow(psi)),
                   method = "regression") {
  covy <- lambda %*% psi %*% t(lambda) + theta
  ginvcovy <- MASS::ginv(covy)
  tlam_invcov <- crossprod(lambda, ginvcovy)
  meany <- lambda %*% alpha + nu
  y1c <- t(as.matrix(y)) - as.vector(meany)
  # Bartlett score
  # t(MASS::ginv(tlam_invcov %*% lambda) %*% tlam_invcov %*% y1c + alpha)
  # Regression score
  t(psi %*% tlam_invcov %*% y1c + as.vector(alpha))
}
