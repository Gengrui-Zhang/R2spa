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

get_fs <- function(data, model = NULL, ...) {
  if (!is.data.frame(data)) data <- as.data.frame(data)
  ind_names <- colnames(data)
  if (is.null(model)) {
    model <- paste("f1 =~",
                   paste(ind_names, collapse = " + "))
  }
  fit <- cfa(model, data = data, ...)
  psi <- lavInspect(fit, what = "est")$psi
  # Need to allow for additional options
  fscore <- lavPredict(fit, se = "standard")
  fscore_se <- attr(fscore, "se")[[1]]
  fscore_rho <- 1 - fscore_se^2 / diag(psi)
  colnames(fscore) <- paste0("fs_", colnames(fscore))
  colnames(fscore_se) <- paste0("fs_", colnames(fscore_se), "_se")
  colnames(fscore_rho) <- paste0("fs_", colnames(fscore_rho), "_rel")
  cbind(as.data.frame(fscore), fscore_se, fscore_rho)
}
