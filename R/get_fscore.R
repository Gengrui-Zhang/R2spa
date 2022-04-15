#' Get Factor Scores and the Corresponding Reliability
#' @param data A data frame containing indicators.
#' @param model An optional string specifying the measurement model
#'              in \code{lavaan} syntax.
#'              See \code{\link[lavaan]{model.syntax}} for more information.
#' @param ... additional arguments passed to \code{\link[lavaan]{cfa}}. See
#'            \code{\link[lavaan]{lavOptions}} for a complete list.
#' @return A data frame containing the factor scores (prefaced with "fs_"),
#'         the standard errors, and the corresponding reliability.
#' @examples
#' library(lavaan)
#' get_fs(PoliticalDemocracy[c("x1", "x2", "x3")])
#'
#' # Multiple-group
#' hs_model <- ' visual  =~ x1 + x2 + x3 '
#' fit <- cfa(hs_model,
#'            data = HolzingerSwineford1939,
#'            group = "school")
#' get_fs(HolzingerSwineford1939, hs_model, group = "school")

get_fs <- function(data, model = NULL, group = NULL, ...) {
  if (!is.data.frame(data)) data <- as.data.frame(data)
  ind_names <- colnames(data)
  if (is.null(model)) {
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
