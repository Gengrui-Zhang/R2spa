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
#' @param corrected_fsT Logical. Whether to correct for the the sampling
#'                      error in the factor score weights when computing
#'                      the error variance estimates of factor scores.
#' @param vfsLT Logical. Whether to return the covariance matrix of `fsT`
#'              and `fsL`, which can be used as input for [vcov_corrected()]
#'              to obtain corrected covariances and standard errors for
#'              [tspa()] results. This is currently ignored.
#' @param ... additional arguments passed to \code{\link[lavaan]{cfa}}. See
#'            \code{\link[lavaan]{lavOptions}} for a complete list.
#' @return A data frame containing the factor scores (with prefix `"fs_"`),
#'         the standard errors (with suffix `"_se"`), the implied loadings
#'         of factor `"_by_"` factor scores, and the error variance-covariance
#'         of the factor scores (with prefix `"evfs_"`). The following are
#'         also returned as attributes:
#'         * `fsT`: error covariance of factor scores
#'         * `fsL`: loading matrix of factor scores
#'         * `fsb`: intercepts of factor scores
#'         * `scoring_matrix`: weights for computing factor scores from items
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
                   corrected_fsT = FALSE,
                   vfsLT = FALSE,
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
  get_fs_lavaan(lavobj = fit, method = method,
                corrected_fsT = corrected_fsT,
                vfsLT = vfsLT)
}

#' @inherit get_fs
#' @param lavobj A lavaan model object when using [get_fs_lavaan()].
#' @export
get_fs_lavaan <- function(lavobj,
                          method = c("regression", "Bartlett"),
                          corrected_fsT = FALSE,
                          vfsLT = FALSE) {
  est <- lavInspect(lavobj, what = "est")
  y <- lavInspect(lavobj, what = "data")
  if (corrected_fsT) {
    add_to_evfs <- correct_evfs(lavobj, method = method)
  } else {
    add_to_evfs <- rep(0, lavInspect(lavobj, what = "ngroups"))
  }
  prepare_fs_dat <- function(y, est, add) {
    fscore <- compute_fscore(y,
                             lambda = est$lambda,
                             theta = est$theta,
                             psi = est$psi,
                             nu = est$nu,
                             alpha = est$alpha,
                             method = method,
                             fs_matrices = TRUE)
    augment_fs(est, fscore, attr(fscore, "fsT") + add)
  }
  group <- lavInspect(lavobj, what = "group")
  if (length(group) == 0) {
    out <- prepare_fs_dat(y, est, add_to_evfs[[1]])
  } else {
    fs_lst <- setNames(
      vector("list", length = length(est)),
      lavobj@Data@group.label
    )
    for (g in seq_along(fs_lst)) {
      fs_lst[[g]] <- prepare_fs_dat(y[[g]], est[[g]], add_to_evfs[[g]])
      fs_lst[[g]][[group]] <- names(est[g])
    }
    attr_names <- setdiff(names(attributes(fs_lst[[1]])),
                          c("names", "class", "row.names", "col.names"))
    attr_lst <- rep(
      list(
        setNames(vector("list", length = length(est)), lavobj@Data@group.label)
      ),
      length(attr_names)
    )
    for (j in seq_along(attr_lst)) {
      for (g in seq_along(fs_lst)) {
        attr_lst[[j]][[g]] <- attr(fs_lst[[g]], which = attr_names[j])
      }
      attr(fs_lst, which = attr_names[j]) <- attr_lst[[j]]
    }
    out <- fs_lst
  }
  if (vfsLT) {
    attr(out, "vfsLT") <- vcov_ld_evfs(lavobj, method = method)
  }
  out
}

augment_fs <- function(est, fs, fs_ev) {
  fs_se <- t(as.matrix(sqrt(diag(fs_ev))))
  # fs_se[is.nan(fs_se)] <- 0
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
  fsL <- attr(fs, "fsL")
  fs_names <- paste0("fs_", colnames(fsL))
  fs_lds <- lapply(seq_len(ncol(fsL)), function(i) {
    setNames(fsL[, i],
             paste(colnames(fsL)[i], fs_names, sep = "_by_"))
  })
  fs_lds <- unlist(fs_lds)
  fs_dat <- cbind(as.data.frame(fs), fs_se, t(as.matrix(fs_lds)),
                  t(as.matrix(fs_evs)))
  attr(fs_dat, "fsT") <- fs_ev
  attr(fs_dat, "fsL") <- fsL
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
#'                    portion of factor scores (\code{fsT}), factor score
#'                    loading matrix (\eqn{L}; \code{fsL}) and intercept vector
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
  a_mat <- compute_a_from_mat(method,
                              lambda = lambda, psi = psi, theta = theta)
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
    fsL <- unclass(a_mat %*% lambda)
    fs_names <- paste0("fs_", colnames(fsL))
    rownames(fsL) <- fs_names
    attr(fs, "fsL") <- fsL
    fsb <- as.numeric(alpha - fsL %*% alpha)
    names(fsb) <- fs_names
    attr(fs, "fsb") <- fsb
    # tv <- fsL %*% psi %*% t(fsL)
    # fsv <- a_mat %*% covy %*% t(a_mat)
    # attr(fs, "fsT") <- fsv - tv
    fsT <- a_mat %*% theta %*% t(a_mat)
    rownames(fsT) <- colnames(fsT) <- fs_names
    attr(fs, "fsT") <- fsT
  }
  fs
}

compute_fspars <- function(par, lavobj, method = c("regression", "Bartlett"),
                           what = c("a", "evfs", "ldfs")) {
  method <- match.arg(method)
  what <- match.arg(what)
  ngrp <- lavInspect(lavobj, what = "ngroups")
  frees <- lavInspect(lavobj, what = "free")
  mats <- lavInspect(lavobj, what = "est")
  if (ngrp == 1) {
    frees <- list(frees)
    mats <- list(mats)
  }
  out <- vector("list", ngrp)
  for (g in seq_len(ngrp)) {
    free <- frees[[g]]
    mat <- mats[[g]]
    free_list <- lapply(free, FUN = \(x) x[which(x > 0)])
    for (l in seq_along(free_list)) {
      for (i in free_list[[l]]) {
        mat[[l]][which(free[[l]] == i)] <- par[i]
      }
    }
    a <- do.call(compute_a_from_mat,
                 args = c(method, mat[c("lambda", "psi", "theta")]))
    if (what == "a") {
      out[[g]] <- a
    } else if (what == "evfs") {
      out[[g]] <- a %*% mat$theta %*% t(a)
    } else if (what == "ldfs") {
      out[[g]] <- a %*% mat$lambda
    }
  }
  out
}

compute_a <- function(par, lavobj, method = c("regression", "Bartlett")) {
  compute_fspars(par, lavobj = lavobj, method = method, what = "a")
}

compute_a_from_mat <- function(method = c("regression", "Bartlett"),
                               lambda, theta, psi = NULL) {
  method <- match.arg(method)
  if (method == "regression") {
    if (is.null(psi)) {
      stop("input of psi (latent covariance) is needed for regression scores")
    }
    compute_a_reg(lambda, theta = theta, psi = psi)
  } else if (method == "Bartlett") {
    compute_a_bartlett(lambda, theta = theta, psi = psi)
  }
}

compute_a_reg <- function(lambda, theta, psi) {
  covy <- lambda %*% psi %*% t(lambda) + theta
  ginvcovy <- MASS::ginv(covy)
  tlam_invcov <- crossprod(lambda, ginvcovy)
  psi %*% tlam_invcov
}

compute_a_bartlett <- function(lambda, theta, psi = NULL) {
  ginvth <- MASS::ginv(theta)
  tlam_invth <- crossprod(lambda, ginvth)
  solve(tlam_invth %*% lambda, tlam_invth)
}

correct_evfs <- function(fit, method = c("regression", "Bartlett")) {
  method <- match.arg(method)
  ngrp <- lavInspect(fit, what = "ngroups")
  est_fits <- lavInspect(fit, what = "est")
  if (ngrp == 1) est_fits <- list(est_fits)
  outs <- vector("list", ngrp)
  for (g in seq_len(ngrp)) {
    est_fit <- est_fits[[g]]
    p <- nrow(est_fit$psi)
    jac_a <- vector("list", length = p)
    for (i in seq_len(p)) {
      jac_a[[i]] <- lavaan::lav_func_jacobian_complex(
        function(x, fit, method) {
          compute_a(x, lavobj = fit, method = method)[[g]][i, ]
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
    outs[[g]] <- out
  }
  outs
}

compute_evfs <- function(par, lavobj, method = c("regression", "Bartlett")) {
  compute_fspars(par, lavobj = lavobj, method = method, what = "evfs")
}

compute_ldfs <- function(par, lavobj, method = c("regression", "Bartlett")) {
  compute_fspars(par, lavobj = lavobj, method = method, what = "ldfs")
}

compute_grad_ld_evfs <- function(fit, method = c("regression", "Bartlett")) {
  method <- match.arg(method)
  lavaan::lav_func_jacobian_complex(
    function(x, fit, method) {
      evfs <- compute_evfs(x, lavobj = fit, method = method)
      evfs_lower <- lapply(evfs, function(x) {
        x[lower.tri(x, diag = TRUE)]
      })
      c(unlist(compute_ldfs(x, lavobj = fit, method = method)),
        unlist(evfs_lower))
    },
    coef(fit),
    fit = fit,
    method = method
  )
}

vcov_ld_evfs <- function(fit, method = c("regression", "Bartlett")) {
  method <- match.arg(method)
  jac <- compute_grad_ld_evfs(fit, method = method)
  jac %*% lavaan::vcov(fit) %*% t(jac)
}