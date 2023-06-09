#' Get Factor Scores and the Corresponding Standard Error of Measurement
#' @param data A data frame containing indicators.
#' @param model An optional string specifying the measurement model
#'              in \code{lavaan} syntax.
#'              See \code{\link[lavaan]{model.syntax}} for more information.
#' @param group Character. Name of the grouping variable for multiple group
#'              analysis, which is passed to \code{\link[lavaan]{cfa}}.
#' @param method Character. Method for computing factor scores (options are
#'               "regression" or "Bartlett"). Currently, the default is
#'               "regression" to be consistent with
#'               \code{\link[lavaan]{lavPredict}}, but the Bartlett scores have
#'               more desirable properties and may be preferred for 2S-PA.
#' @param corrected_av_efs Logical. Whether to correct for the the sampling
#'                         error in the factor score weights when computing
#'                         the error variance estimates of factor scores.
#' @param ... additional arguments passed to \code{\link[lavaan]{cfa}}. See
#'            \code{\link[lavaan]{lavOptions}} for a complete list.
#' @return A data frame containing the factor scores (with prefix "fs_") and
#'         the standard errors (with suffix "_se").
#'
#' @importFrom lavaan cfa sem lavInspect lavTech coef
#' @importFrom stats setNames
#'
#' @export
#'
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

get_fs <- function(data, model = NULL, group = NULL,
                   method = c("regression", "Bartlett"),
                   corrected_av_efs = FALSE,
                   ...) {
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
  y <- lavInspect(fit, what = "data")
  if (corrected_av_efs) {
    add_to_evfs <- correct_evfs(fit, method = method)
  } else {
    add_to_evfs <- 0
  }
  prepare_fs_dat <- function(y, est) {
    fscore <- compute_fscore(y,
                             lambda = est$lambda,
                             theta = est$theta,
                             psi = est$psi,
                             nu = est$nu,
                             alpha = est$alpha,
                             method = method,
                             fs_matrices = TRUE)
    augment_fs(est, fscore, attr(fscore, "av_efs") + add_to_evfs)
  }
  if (is.null(group)) {
    prepare_fs_dat(y, est)
  } else {
    fs_lst <- av_efs_lst <- fsA_lst <- fsb_lst <- fsS_lst <-
      setNames(vector("list", length = length(est)),
               fit@Data@group.label)
    for (i in seq_along(fs_lst)) {
      fs_lst[[i]] <- prepare_fs_dat(y[[i]], est[[i]])
      fs_lst[[i]][[group]] <- names(est[i])
      av_efs_lst[[i]] <- attr(fs_lst[[i]], "av_efs")
      fsA_lst[[i]] <- attr(fs_lst[[i]], "fsA")
      fsb_lst[[i]] <- attr(fs_lst[[i]], "fsb")
      fsS_lst[[i]] <- attr(fs_lst[[i]], "scoring_matrix")
    }
    attr(fs_lst, which = "av_efs") <- av_efs_lst
    attr(fs_lst, which = "fsA") <- fsA_lst
    attr(fs_lst, which = "fsb") <- fsb_lst
    attr(fs_lst, which = "scoring_matrix") <- fsS_lst
    fs_lst
  }
}

augment_fs <- function(est, fs, fs_ev) {
  fs_se <- t(as.matrix(sqrt(diag(fs_ev))))
  colnames(fs) <- paste0("fs_", colnames(fs))
  colnames(fs_se) <- paste0(colnames(fs_se), "_se")
  num_lvs <- ncol(fs_ev)
  fs_evs <- rep(NA, num_lvs * (num_lvs + 1) / 2)
  count <- 1
  for (i in seq_len(num_lvs)) {
    for (j in seq_len(i)) {
      fs_evs[count] <- fs_ev[i, j]
      if (i == j) {
        names(fs_evs)[count] <- paste0("ev_", rownames(fs_ev)[i])
      } else {
        names(fs_evs)[count] <- paste0("ecov_",
                                       rownames(fs_ev)[i], "_",
                                       colnames(fs_ev)[j])
      }
      count <- count + 1
    }
  }
  fsA <- attr(fs, "fsA")
  fs_names <- paste0("fs_", colnames(fsA))
  fs_lds <- lapply(seq_len(ncol(fsA)), function(i) {
    setNames(fsA[, i],
             paste(colnames(fsA)[i], fs_names, sep = "_by_"))
  })
  fs_lds <- unlist(fs_lds)
  fs_dat <- cbind(as.data.frame(fs), fs_se, t(as.matrix(fs_lds)),
                  t(as.matrix(fs_evs)))
  attr(fs_dat, "av_efs") <- fs_ev
  attr(fs_dat, "fsA") <- fsA
  attr(fs_dat, "fsb") <- attr(fs, "fsb")
  attr(fs_dat, "scoring_matrix") <- attr(fs, "scoring_matrix")
  fs_dat
}

#' Compute factor scores
#'
#' @param y An N x p matrix where each row is a response vector. If there
#'          is only one observation, it should be a matrix of one row.
#' @param lambda A p x q matrix of factor loadings.
#' @param theta A p x p matrix of unique variance-covariances.
#' @param psi A q x q matrix of latent factor variance-covariances.
#' @param nu A vector of length p of measurement intercepts.
#' @param alpha A vector of length q of latent means.
#' @param method A character string indicating the method for computing factor
#'               scores. Currently, only "regression" is supported.
#' @param center_y Logical indicating whether \code{y} should be mean-centered.
#'                 Default to \code{TRUE}.
#' @param fs_matrices Logical indicating whether covariances of the error
#'                    portion of factor scores (\code{av_efs}), factor score
#'                    loading matrix (\eqn{A}; \code{fsA}) and intercept vector
#'                    (\eqn{b}; \code{fsb}) should be returned.
#'                    The loading and intercept matrices are the implied
#'                    loadings and intercepts by the model when using the
#'                    factor scores as indicators of the latent variables.
#'                    If \code{TRUE}, these matrices will be added as
#'                    attributes.
#' @param acov Logical indicating whether the asymptotic covariance matrix
#'             of factor scores should be returned as an attribute.
#'
#' @return An N x p matrix of factor scores.
#' @export
#'
#' @examples
#' library(lavaan)
#' fit <- cfa(" ind60 =~ x1 + x2 + x3
#'              dem60 =~ y1 + y2 + y3 + y4 ",
#'            data = PoliticalDemocracy)
#' fs_lavaan <- lavPredict(fit, method = "Bartlett")
#' # Using R2spa::compute_fscore()
#' est <- lavInspect(fit, what = "est")
#' fs_hand <- compute_fscore(lavInspect(fit, what = "data"),
#'                           lambda = est$lambda,
#'                           theta = est$theta,
#'                           psi = est$psi,
#'                           method = "Bartlett")
#' fs_hand - fs_lavaan  # same scores
compute_fscore <- function(y, lambda, theta, psi = NULL,
                           nu = NULL, alpha = NULL,
                           method = c("regression", "Bartlett"),
                           center_y = TRUE,
                           acov = FALSE,
                           fs_matrices = FALSE) {
  method <- match.arg(method)
  if (is.null(nu)) nu <- colMeans(y)
  if (is.null(alpha)) alpha <- matrix(0, nrow = ncol(as.matrix(lambda)))
  y1c <- t(as.matrix(y))
  if (center_y) {
    meany <- lambda %*% alpha + nu
    y1c <- y1c - as.vector(meany)
  }
  if (method == "regression") {
    if (is.null(psi)) {
      stop("input of psi (latent covariance) is needed for regression scores")
    }
    # Regression score
    a_mat <- compute_a_reg(lambda, psi, theta)
  } else if (method == "Bartlett") {
    # Bartlett score
    a_mat <- compute_a_bartlett(lambda, theta)
  }
  fs <- t(a_mat %*% y1c + as.vector(alpha))
  if (acov) {
    # if (is.null(psi)) {
    #   stop("input of psi (latent covariance) is needed for acov")
    # }
    if (method == "regression") {
      covy <- lambda %*% psi %*% t(lambda) + theta
      attr(fs, "acov") <-
        unclass(psi - a_mat %*% covy %*% t(a_mat))
    } else if (method == "Bartlett") {
      attr(fs, "acov") <-
        unclass(a_mat %*% theta %*% t(a_mat))
    }
  }
  if (fs_matrices) {
    attr(fs, "scoring_matrix") <- a_mat
    fsA <- unclass(a_mat %*% lambda)
    fs_names <- paste0("fs_", colnames(fsA))
    rownames(fsA) <- fs_names
    attr(fs, "fsA") <- fsA
    fsb <- as.numeric(alpha - fsA %*% alpha)
    names(fsb) <- fs_names
    attr(fs, "fsb") <- fsb
    # tv <- fsA %*% psi %*% t(fsA)
    # fsv <- a_mat %*% covy %*% t(a_mat)
    # attr(fs, "av_efs") <- fsv - tv
    av_efs <- a_mat %*% theta %*% t(a_mat)
    rownames(av_efs) <- colnames(av_efs) <- fs_names
    attr(fs, "av_efs") <- av_efs
  }
  fs
}

compute_a <- function(par, lavobj, method = c("regression", "Bartlett")) {
  method <- match.arg(method)
  free <- lavInspect(lavobj, what = "free")
  free_list <- lapply(free, FUN = \(x) x[which(x > 0)])
  mat <- lavInspect(lavobj, what = "est")
  for (l in seq_along(free_list)) {
    for (i in free_list[[l]]) {
      mat[[l]][which(free[[l]] == i)] <- par[i]
    }
  }
  if (method == "regression") {
    return(do.call(compute_a_reg, args = mat[c("lambda", "psi", "theta")]))
  } else if (method == "Bartlett") {
    return(do.call(compute_a_bartlett, args = mat[c("lambda", "theta")]))
  }
}

compute_a_reg <- function(lambda, psi, theta) {
  covy <- lambda %*% psi %*% t(lambda) + theta
  ginvcovy <- MASS::ginv(covy)
  tlam_invcov <- crossprod(lambda, ginvcovy)
  psi %*% tlam_invcov
}

compute_a_bartlett <- function(lambda, theta) {
  ginvth <- MASS::ginv(theta)
  tlam_invth <- crossprod(lambda, ginvth)
  solve(tlam_invth %*% lambda, tlam_invth)
}

correct_evfs <- function(fit, method = c("regression", "Bartlett")) {
  est_fit <- lavInspect(fit, what = "est")
  p <- nrow(est_fit$psi)
  jac_a <- vector("list", length = p)
  for (i in seq_len(p)) {
    jac_a[[i]] <- lavaan::lav_func_jacobian_complex(
      function(x, fit, method) {
        compute_a(x, lavobj = fit, method = method)[i, ]
      },
      coef(fit),
      fit = fit,
      method = method
    )
  }
  out <- matrix(nrow = p, ncol = p)
  th <- est_fit$theta
  vc_fit <- vcov(fit)
  for (j in seq_len(p)) {
    for (i in j:p) {
      out[i, j] <- sum(diag(th %*% jac_a[[i]] %*% vc_fit %*% t(jac_a[[j]])))
      if (i > j) {
        out[j, i] <- out[i, j]
      }
    }
  }
  out
}
