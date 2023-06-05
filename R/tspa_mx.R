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
  if (!isTRUE(attr(class(mat_ld), which = "package") == "OpenMx")) {
    fs_name <- mx_model$manifestVars
    ld_ind <- match(fs_name, table = rownames(mat_ld))
    if (any(is.na(ld_ind))) {
      stop("`rownames(mat_ld)` must match the names of the",
           "factor score variables.")
    }
    mat_ld <- make_mx_ld(mat_ld[ld_ind, ld_ind])
  }
  if (!isTRUE(attr(class(mat_vc), which = "package") == "OpenMx")) {
    fs_name <- mx_model$manifestVars
    vc_ind <- match(fs_name, table = rownames(mat_vc))
    if (any(is.na(vc_ind))) {
      stop("`rownames(mat_vc)` must match the names of the",
           "factor score variables.")
    }
    mat_vc <- make_mx_vc(mat_vc[vc_ind, vc_ind])
  }
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

make_mx_ld <- function(ld_mat) {
  if (is.numeric(ld_mat)) {
    val <- ld_mat
    lab <- NA
  } else if (is.character(ld_mat)) {
    lab <- ld_mat
    lab[!is.na(lab)] <- paste0("data.", lab[!is.na(lab)])
    val <- NA
  } else {
    stop("`ld_mat` must be either a numeric matrix or a character matrix.")
  }
  mxMatrix(
    type = "Full", nrow = nrow(ld_mat), ncol = nrow(ld_mat),
    free = FALSE,
    values = val,
    labels = lab,
    name = "L"
  )
}

make_mx_vc <- function(vc_mat) {
  if (is.numeric(vc_mat)) {
    val <- vc_mat
    lab <- NA
  } else if (is.character(vc_mat)) {
    lab <- vc_mat
    lab[!is.na(lab)] <- paste0("data.", lab[!is.na(lab)])
    val <- NA
  } else {
    stop("`vc_mat` must be either a numeric matrix or a character matrix.")
  }
  mxMatrix(
    type = "Symm", nrow = nrow(vc_mat), ncol = nrow(vc_mat),
    free = FALSE,
    values = val,
    labels = lab,
    name = "E"
  )
}