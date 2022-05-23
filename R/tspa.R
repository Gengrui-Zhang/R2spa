#' Two-Stage Path Analysis
#' @param model A string variable describing the structural path model.
#' @param data A dataframe containing factor scores.
#' @param reliability A numeric vector representing the reliability indexes
#'   of each latent factor.
#' @examples
#' library(lavaan)
#' ### single-group example
#'
#' # cfa model
#' my_cfa <- '
#' # latent variables
#' ind60 =~ x1 + x2 + x3
#' dem60 =~ y1 + y2 + y3 + y4
#' '
#' cfa_fit <- cfa(model = my_cfa,
#'                data  = PoliticalDemocracy)
#'
#' # get factor scores
#' fs_dat <- get_fs(PoliticalDemocracy, my_cfa)
#'
#' # tspa model
#' tspa(model = "dem60 ~ ind60", data = fs_dat,
#'      se = data.frame(ind60 = 0.1273703, dem60 = 0.6761707))
#'
#'
#' ### three-variable single-group example
#'
#' # cfa model
#' cfa_3var <- '
#' # latent variables
#' ind60 =~ x1 + x2 + x3
#' dem60 =~ y1 + y2 + y3 + y4
#' dem65 =~ y5 + y6 + y7 + y8
#'
#' # residual correlations
#' y1 ~~ y5
#' y2 ~~ y4 + y6
#' y3 ~~ y7
#' y4 ~~ y8
#' y6 ~~ y8
#' '
#'
#' # get factor scores
#' fs_3var_dat <- get_fs(PoliticalDemocracy, cfa_3var)
#'
#' # tspa model
#' tspa(model = "dem60 ~ ind60
#'               dem65 ~ ind60 + dem60",
#'      data = fs_3var_dat,
#'      se = data.frame(ind60 = 0.1267792, dem60 = 0.6863648, dem65 = 0.6074362))
#'
#'
#' ### multigroup example
#'
#' hs_mod <- '
#' visual =~ x1 + x2 + x3
#' speed =~ x7 + x8 + x9
#' '
#'
#' # get factor scores
#' fs_hs <- get_fs(HolzingerSwineford1939, hs_mod, group = "school")
#'
#' # tspa model
#' tspa(model = "visual ~ speed",
#'      data = fs_hs,
#'      se = data.frame(visual = c(0.4284448, 0.4128444),
#'                      speed = c(0.3271431, 0.3037251)),
#'      group = "school",
#'      group.equal = "regressions")
#'
#' # manually adding equality constraints on the regression coefficients
#' tspa(model = "visual ~ c(b1, b1) * speed",
#'      data = fs_hs,
#'      se = list(visual = c(0.4284448, 0.4128444),
#'                speed = c(0.3271431, 0.3037251)),
#'      group = "school")




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



tspa <- function(model, data, reliability = NULL, se = NULL, ...) {
<<<<<<< HEAD

=======
  if (!is.data.frame(se)) {
    se <- as.data.frame(as.list(se))
  }
>>>>>>> 73c4262ac677f5325a6ad98dfe1f9cecba245193
    if(nrow(se) == 1){
        tspaModel <- tspaSingleGroup(model, data, se)
    }
    else if(nrow(se) > 1){
        tspaModel <- tspaMultipleGroupSe(model, data, se)
    }

    tspa_fit <- sem(model = tspaModel,
                    data  = data,
                    ...)
    attributes(tspa_fit)$tspaModel <- tspaModel # to access the attribute, use attr(x,"tspaModel")
    return (tspa_fit)
#
#   var <- names(reliability)
#   len <- length(reliability)
#   group <- length(reliability[1])
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
#   # if(fs %in% col == FALSE) {
#   #   stop("data columns does not match with reliability")
#   # }
#
#   for (x in 1:len) {
#       # latent variables fx =~ c(1, 1, 1, 1) * fs_fx
#       latent_var[x] <- paste0(var[x], "=~ c(", paste0(rep(1, group), collapse = ", "), ") * ", fs[x], "\n")
#
#       # constrain the errors fs_fx ~~ c(ev11, ev12, ev13, ev14) * fs_fx
#       error_constraint[x] <- paste0(fs[x], "~~ c(", paste0(paste0(rep(paste0("ev",x), group), 1:group), collapse = ", "), ") * ", fs[x], "\n")
#
#       # latent variances fx ~~ c(v11, v12, v13, v14) * fx
#       latent_variance[x] <- paste0(var[x], " ~~ c(", paste0(paste0(rep(paste0("v",x), group), 1:group), collapse = ", "), ") * ", var[x], "\n")
#
#       # reliability constraints v21 == (0.8854708 / (1 - 0.8854708)) * ev21 - b1^2 * v11
#       if(x == 1) {
#           reliability_constraint[x] <- paste0("v", x, 1:group, " == ", toString(reliability[x]), " / ", toString(1 - reliability[x]),
#                                             " * ev", x, 1:group, "\n")
#       }
#       else {
#           reliability_constraint[x] <- paste0("v", x, 1:group, " == ", toString(reliability[x]), " / ", toString(1 - reliability[x]),
#                                           " * ev", x, 1:group, "- b", x - 1, "^2 * v", x - 1, 1:group, "\n")
#       }
#   }
#
#   # for now we only assume there's fx and fy, fy ~ c(b1, b1, b1, b1) * fx1 + c(b1, b1, b1, b1) * fx2
#   model <- paste0(var[2]," ~ c(", paste0(rep("b1", group), collapse = ", "), ") * ", var[1])
#
#   # organize into one tspa model
#   latent_var_str <- paste(latent_var, collapse="")
#   error_constraint_str <- paste(error_constraint, collapse="")
#   latent_variance_str <- paste(latent_variance, collapse="")
#   reliability_constraint_str <- paste(reliability_constraint, collapse="")
#   tspaModel <- paste0(latent_var_str, error_constraint_str, latent_variance_str, model, "\n", reliability_constraint_str)
#
#   # tspa_fit, fit the model for the data
#   tspa_fit <- sem(model = tspaModel,
#   # sem(model = my2spa_lui,
#   #     data  = fs_lui,
#   #     group = "eth",
#   #     group.label = 1:4)
#                   data  = data,
#                   ...)
#
#   # tspaModel <- cat(tspaModel)
#   attributes(tspa_fit)$tspaModel <- tspaModel
#   # to access the attribute, use attr(x,"tspaModel")
#   return (tspa_fit)
}

tspaSingleGroup <- function(model, data, se = NULL) {
    if (nrow(se) != 0){
        ev <- se^2
        var <- names(se)
        len <- length(se)

        col <- colnames(data)
        fs <- paste0("fs_", var)
        latent_var <- rep(NA, len)
        error_constraint <- rep(NA, len)
        latent_variance <- rep(NA, len)

        for(x in 1:len){
          latent_var[x] <- paste0(var[x], " =~ ", fs[x], "\n")
          error_constraint[x] <- paste0(fs[x], " ~~ ", ev[x], " * ", fs[x], "\n")
          latent_variance[x] <- paste0(var[x], " ~~ v", x, " * ", var[x], "\n")
        }

        latent_var_str <- paste(latent_var, collapse="")
        error_constraint_str <- paste(error_constraint, collapse="")
        latent_variance_str <- paste(latent_variance, collapse="")
        tspaModel <- paste0(latent_var_str, error_constraint_str, latent_variance_str, model, "\n")

        return (tspaModel)
    }
}


tspaMultipleGroupSe <- function(model, data, se = NULL) {
  # if (is.list(se)) {
  #   len <-
  # }
  if (nrow(se) != 0){
      ev <- se^2
      var <- names(se)
      len <- length(se)
      group <- nrow(se)
      col <- colnames(data)
      fs <- paste0("fs_", var)
      tspaModel <- rep(NA, len)
      latent_var <- rep(NA, len)
      error_constraint <- rep(NA, len)
      latent_variance <- rep(NA, len)

      for(x in 1:len){
        latent_var[x] <- paste0(var[x], "=~ c(", paste0(rep(1, group), collapse = ", "), ") * ", fs[x], "\n")
        error_constraint[x] <- paste0(fs[x], "~~ c(", paste(ev[x], collapse = ", "), ") * ", fs[x], "\n")
        latent_variance[x] <- paste0(var[x], " ~~ c(", paste0(paste0(rep(paste0("v",x), group), 1:group), collapse = ", "), ") * ", var[x], "\n")
      }

      latent_var_str <- paste(latent_var, collapse="")
      error_constraint_str <- paste(error_constraint, collapse="")
      latent_variance_str <- paste(latent_variance, collapse="")
      tspaModel <- paste0(latent_var_str, error_constraint_str, latent_variance_str, model, "\n")

      return (tspaModel)
  }
}
