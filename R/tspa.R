#' Two-Stage Path Analysis
#' @param model A string variable describing the structural path model.
#' @param data A dataframe containing factor scores.
#' @param reliability A numeric vector representing the reliability indexes
#'   of each latent factor.
#' @examples
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
#' # create a data frame for factor scores
#' fs_dat <- lavPredict(cfa_fit, se = "standard")
#' colnames(fs_dat) <- c("fs_ind60", "fs_dem60")
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
#' # residual correlations
#' y1 ~~ y5
#' y2 ~~ y4 + y6
#' y3 ~~ y7
#' y4 ~~ y8
#' y6 ~~ y8
#' '
#' cfa_3var_fit <- cfa(model = cfa_3var,
#'                     data  = PoliticalDemocracy)
#'
#' # create a data frame for factor scores
#' fs_3var_dat <- lavPredict(cfa_3var_fit, se = "standard")
#' colnames(fs_3var_dat) <- c("fs_ind60", "fs_dem60", "fs_dem65")
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
#' # partial scalar model
#' ps_mod <- '
#' fx =~ class1 + class2 + class3 + class4 + class5 + class6 +
#'       class7 + class8 + class9 + class10 + class11 + class12 +
#'       class13 + class14 + class15
#' fy =~ audit1 + audit2 + audit3
#'
#' class1 ~ c(i1, i1, i1, i1.h)*1
#' class2 ~ c(i2, i2, i2, i2.h)*1
#' class4 ~ c(i4.w, i4, i4, i4)*1
#' class6 ~ c(i6, i6, i6.b, i6)*1
#' class10 ~ c(i10, i10.a, i10, i10)*1
#' class11 ~ c(i11, i11, i11, i11.h)*1
#' class12 ~ c(i12, i12.a, i12, i12)*1
#' class13 ~ c(i13, i13, i13.b, i13)*1
#' class14 ~ c(i14.w, i14, i14, i14)*1
#' class15 ~ c(i15, i15.a, i15, i15)*1
#' '
#' psfit_eth <- cfa(model = ps_mod,
#'                  data = lui2018,
#'                  group = "eth",
#'                  group.label = 1:4,
#'                  group.equal = c("loadings", "intercepts"),
#'                  std.lv = TRUE)
#'
#' # create a data frame for factor scores
#' fs_lui <- cbind(do.call(rbind, lavPredict(psfit_eth, se = "standard")),
#' rep(1:4, table(lui2018$eth)))
#' colnames(fs_lui) <- c("fs_fx", "fs_fy", "eth")
#'
#' # tspa model
#' tspa(model = "fy ~ fx", data = fs_lui,
#'      se = data.frame(fy = c(0.07196033, 0.07581671, 0.08773593, 0.08275693),
#'                      fx = c(0.11452919, 0.06107097, 0.08913058, 0.08796366)),
#'      group = "eth",
#'      group.equal = "regressions")




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

  # if(fs %in% col == FALSE) {
  #   stop("data columns does not match with reliability")
  # }

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

  # for now we only assume there's fx and fy, fy ~ c(b1, b1, b1, b1) * fx1 + c(b1, b1, b1, b1) * fx2
  model <- paste0(var[2]," ~ c(", paste0(rep("b1", group), collapse = ", "), ") * ", var[1])

  # organize into one tspa model
  latent_var_str <- paste(latent_var, collapse="")
  error_constraint_str <- paste(error_constraint, collapse="")
  latent_variance_str <- paste(latent_variance, collapse="")
  reliability_constraint_str <- paste(reliability_constraint, collapse="")
  tspaModel <- paste0(latent_var_str, error_constraint_str, latent_variance_str, model, "\n", reliability_constraint_str)

  # tspa_fit, fit the model for the data
  tspa_fit <- sem(model = tspaModel,
  # sem(model = my2spa_lui,
  #     data  = fs_lui,
  #     group = "eth",
  #     group.label = 1:4)
                  data  = data,
                  ...)

  # tspaModel <- cat(tspaModel)
  attributes(tspa_fit)$tspaModel <- tspaModel
  # to access the attribute, use attr(x,"tspaModel")
  return (tspa_fit)
}

