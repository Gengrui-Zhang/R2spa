tspa_mx_model <- function(mx_model, data, mat_ld, mat_vc) {
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
    mxExpectationNormal(
      covariance = "expCov", means = "expMean",
      dimnames = mx_model$manifestVars),
    mxFitFunctionML()
  )
}
