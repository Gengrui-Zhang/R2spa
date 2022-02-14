#' Get Factor Scores and the Corresponding Reliability
#' @param data A data frame containing indicators.
#' @param model An optional string specifying the measurement model
#'              in \code{lavaan} syntax.
#'              See \code{\link[lavaan]{model.syntax}} for more information.
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
  rho <- 1 - fscore_se^2 / diag(psi)
  colnames(fscore) <- paste0("fs_", colnames(fscore))
  colnames(rho) <- paste0("fs_", colnames(rho), "_se")
  cbind(as.data.frame(fscore), rho)
}
