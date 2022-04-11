#' Two-Stage Path Analysis
#' @param model A string variable describing the structural path model.
#' @param data A dataframe containing factor scores.
#' @param reliability A numeric vector representing the reliability indexes
#'   of each latent factor.
#' @examples
#' tspa(model = "dem60 ~ ind60", data = fs_dat,
#'      reliability = c(ind60 = 0.9651282, dem60 = 0.9055203))


# # modify for multiple group tspa model
# tspa <- function(model, data, reliability = NULL, se = NULL) {
#   var <- names(reliability)
#   len <- length(reliability)
#
#   if (len < 2) {
#     stop("Reliability len is smaller than 2, unable to build model. Reliability needs to consist of at least 2 variables.");
#   }
#   col <- colnames(data)
#   fs <- paste0("fs_", var)
#
#   if (length(fs) < 2) {
#     stop("Data column len is smaller than 2, unable to build model. Data needs to consist of at least 2 columns.");
#   }
#
#   tspaModel <- rep(NA, len)
#   latent_var <- rep(NA, len)
#   error_constraint <- rep(NA, len)
#   latent_variance <- rep(NA, len)
#   reliability_constraint <- rep(NA, len)
#
#   if(fs %in% col == FALSE) {
#     stop("data columns does not match with reliability")
#   }
#
#   for (x in 1:len) {
#     latent_var[x] <- paste0(var[x], " =~ 1 * ", fs[x], "\n")
#     error_constraint[x] <- paste0(fs[x], " ~~ ev", x, " * ", fs[x], "\n")
#     latent_variance[x] <- paste0(var[x], " ~~ v", x, " * ", var[x], "\n")
#     reliability_constraint[x] <- paste0("v", x, " == ", toString(reliability[x]), " / ", toString(1 - reliability[x]), " * ev", x, "\n")
#   }
#
#   latent_var_str <- paste(latent_var, collapse="")
#   error_constraint_str <- paste(error_constraint, collapse="")
#   latent_variance_str <- paste(latent_variance, collapse="")
#   reliability_constraint_str <- paste(reliability_constraint, collapse="")
#   tspaModel <- paste0(latent_var_str, error_constraint_str, latent_variance_str, model, "\n", reliability_constraint_str)
#
#   tspa_fit <- sem(model = tspaModel,
#                   data  = data)
#
#   # tspaModel <- cat(tspaModel)
#   attributes(tspa_fit)$tspaModel <- tspaModel
#   # to access the attribute, use attr(x,"tspaModel")
#   return (tspa_fit)
# }


tspa <- function(model, data, reliability = NULL, se = NULL) {
  var <- names(reliability)
  len <- length(reliability)
  group <- length(reliability[1])

  if (len < 2) {
    stop("Reliability len is smaller than 2, unable to build model. Reliability needs to consist of at least 2 variables.");
  }
  col <- colnames(data)
  fs <- paste0("fs_", var)

  if (length(fs) < 2) {
    stop("Data column len is smaller than 2, unable to build model. Data needs to consist of at least 2 columns.");
  }

  tspaModel <- rep(NA, len)
  latent_var <- rep(NA, len)
  error_constraint <- rep(NA, len)
  latent_variance <- rep(NA, len)
  reliability_constraint <- rep(NA, len)

  if(fs %in% col == FALSE) {
    stop("data columns does not match with reliability")
  }

  for (x in 1:len) {
      # latent variables fx =~ c(1, 1, 1, 1) * fs_fx
      latent_var[x] <- paste0(var[x], "=~ c(", paste0(rep(1, group), collapse = ", "), ") * ", fs[x], "\n")

      # constrain the errors fs_fx ~~ c(ev11, ev12, ev13, ev14) * fs_fx
      error_constraint[x] <- paste0(fs[x], "~~ c(", paste0(paste0(rep(paste0("ev",x), group), 1:group), collapse = ", "), ") * ", fs[x], "\n")

      # latent variances fx ~~ c(v11, v12, v13, v14) * fx
      latent_variance[x] <- paste0(var[x], " ~~ c(", paste0(paste0(rep(paste0("v",x), group), 1:group), collapse = ", "), ") * ", var[x], "\n")

      # reliability constraints v21 == (0.8854708 / (1 - 0.8854708)) * ev21 - b1^2 * v11
      if(x == 1) {
          reliability_constraint[x] <- paste0("v", x, 1:group, " == ", toString(reliability[x]), " / ", toString(1 - reliability[x]),
                                            " * ev", x, 1:group, "\n")
      }
      else {
          reliability_constraint[x] <- paste0("v", x, 1:group, " == ", toString(reliability[x]), " / ", toString(1 - reliability[x]),
                                          " * ev", x, 1:group, "- b", x - 1, "^2 * v", x - 1, 1:group, "\n")
      }
  }

  # for now we only assume there's fx and fy, fy ~ c(b1, b1, b1, b1) * fx
  model <- paste0("fy ~ c(", paste0(rep("b1", group), collapse = ", "), ") * fx")

  # organize into one tspa model
  latent_var_str <- paste(latent_var, collapse="")
  error_constraint_str <- paste(error_constraint, collapse="")
  latent_variance_str <- paste(latent_variance, collapse="")
  reliability_constraint_str <- paste(reliability_constraint, collapse="")
  tspaModel <- paste0(latent_var_str, error_constraint_str, latent_variance_str, model, "\n", reliability_constraint_str)

  # tspa_fit, fit the model for the data
  tspa_fit <- sem(model = tspaModel,
                  data  = data)

  # tspaModel <- cat(tspaModel)
  attributes(tspa_fit)$tspaModel <- tspaModel
  # to access the attribute, use attr(x,"tspaModel")
  return (tspa_fit)
}

