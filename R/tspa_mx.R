#' Two-Stage Path Analysis with Definition Variables Using OpenMx
#'
#' Fit a two-stage path analysis (2S-PA) model.
#'
#' @param mxModel A model object of class [OpenMx::MxRAMModel-class], created by
#'                [OpenMx::mxModel()] or the [umx] package. This
#'                is the structural model part.
#' @param data A data frame containing factor scores.
#' @param mat_ld A \eqn{p \times p} matrix indicating the loadings of the
#'               factor scores on the latent variables. The *i*th row indicate
#'               loadings of the *i*th factor score variable on the latent
#'               variables. This could be one of the following:
#' * A matrix created by [OpenMx::mxMatrix()] with loading
#'   values.
#' * A named numeric matrix, with rownames and column names
#'   matching the factor score and latent variables.
#' * A named character matrix, with rownames and column names
#'   matching the factor score and latent variables, and the
#'   character values indicating the variable names in `data` for
#'   the corresponding loadings.
#' @param mat_vc Similar to `mat_ld` but for the error variance-covariance
#'               matrix of the factor scores.
#' @param ... Additional arguments passed to [OpenMx::mxModel()].
#' @return An object of class [OpenMx::MxModel-class]. Note that the
#'         model has not been run.
tspa_mx_model <- function(mx_model, data, mat_ld, mat_vc, ...) {
  str_name <- mx_model$name
  p <- ncol(mat_vc)
  mxModel(
    "2SPAD",
    mxData(observed = data, type = "raw"),
    mx_model, mat_ld, mat_vc,
    mxMatrix(type = "Iden", nrow = p, ncol = p, name = "I"),
    mxAlgebraFromString(paste0("(L %*% solve(I - ", str_name, ".A)) %&% ",
                               str_name, ".S + E"), name = "expCov"),
    mxAlgebraFromString(paste0(str_name, ".M %*% t(L %*% solve(I - ",
                               str_name, ".A))"), name = "expMean"),
    # mxAlgebraFromString(paste0(str_name, ".M"), name = "expMean"),
    mxExpectationNormal(
      covariance = "expCov", means = "expMean",
      dimnames = mx_model$manifestVars),
    mxFitFunctionML()
  )
}
