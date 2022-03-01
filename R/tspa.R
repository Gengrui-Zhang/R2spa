#' Two-Stage Path Analysis
#' @param model A string variable describing the structural path model.
#' @param data A dataframe containing factor scores.
#' @param reliability A numeric vector representing the reliability indexes
#'   of each latent factor.
#' @example
#' tspa(model = "dem60 ~ ind60", data = fs_dat,
#'      reliability = c(ind60 = 0.9651282, dem60 = 0.9055203))

tspa <- function(model, data, reliability = NULL) {
  var <- names(reliability)
  len <- length(reliability)
  fs <- colnames(data)

  tspaModel <- rep(NA, len)
  latent_var <- rep(NA, len)
  error_constraint <- rep(NA, len)
  latent_variance <- rep(NA, len)
  reliability_constraint <- rep(NA, len)

  for (x in 1:len) {
    latent_var[x] <- paste0(var[x], ' =~ 1 * ', fs[x], '\n')
    error_constraint[x] <- paste0(fs[x], ' ~~ ev', x, ' * ', fs[x], '\n')
    latent_variance[x] <- paste0(var[x], ' ~~ v', x, ' * ', var[x], '\n')
    reliability_constraint[x] <- paste0('v', x, ' == ', toString(reliability[x]), ' / ', toString(1 - reliability[x]), ' * ev', x, '\n')
  }

  latent_var_str <- paste(latent_var, collapse="")
  error_constraint_str <- paste(error_constraint, collapse="")
  latent_variance_str <- paste(latent_variance, collapse="")
  reliability_constraint_str <- paste(reliability_constraint, collapse="")
  tspaModel <- paste0(latent_var_str, error_constraint_str, latent_variance_str, model, '\n', reliability_constraint_str)

  tspa_fit <- sem(model = tspaModel,
                  data  = data)
  return (list(tspa_fit, tspaModel))
}

