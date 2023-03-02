#' Two-Stage Path Analysis
#'
#' Fit a two-stage path analysis (2S-PA) model.
#'
#' @param model A string variable describing the structural path model,
#'              in \code{lavaan} syntax.
#' @param data A data frame containing factor scores.
#' @param reliability A numeric vector representing the reliability indexes
#'                    of each latent factor. Currently \code{tspa()} does not
#'                    support the reliability argument. Please use \code{se}.
#' @param se A numeric vector representing the standard errors of each factor
#'           score variable for single-group 2S-PA. A list or data frame
#'           storing the standard errors of each group in each latent factor
#'           for multigroup 2S-PA.
#' @param vc An error variance-covariance matrix of the factor scores, which
#'           can be obtained from the output of \code{get_fs()} using
#'           \code{attr()} with the argument \code{which = "av_efs"}.
#' @param cross_loadings A matrix of loadings and cross-loadings from the
#'                       latent variables to the factor scores \code{fs}, which
#'                       can be obtained from the output of \code{get_fs()}
#'                       using \code{attr()} with the argument
#'                       \code{which = "fsA"}.
#'                       For details see the multiple-factors vignette:
#'                       \code{vignette("multiple-factors", package = "R2spa")}.
#' @param ... Additional arguments passed to \code{\link[lavaan]{sem}}. See
#'            \code{\link[lavaan]{lavOptions}} for a complete list.
#' @return An object of class \code{lavaan}, with an attribute \code{tspaModel}
#'         that contains the model syntax.
#' @export
#' @examples
#' library(lavaan)
#'
#' # single-group, two-factor example, factor scores obtained separately
#' # get factor scores
#' fs_dat_ind60 <- get_fs(data = PoliticalDemocracy,
#'                        model = "ind60 =~ x1 + x2 + x3")
#' fs_dat_dem60 <- get_fs(data = PoliticalDemocracy,
#'                        model = "dem60 =~ y1 + y2 + y3 + y4")
#' fs_dat <- cbind(fs_dat_ind60, fs_dat_dem60)
#' # tspa model
#' tspa(model = "dem60 ~ ind60", data = fs_dat,
#'      se = c(ind60 = fs_dat_ind60[1, "fs_ind60_se"],
#'             dem60 = fs_dat_dem60[1, "fs_dem60_se"]))
#'
#' # single-group, three-factor example
#' mod2 <- "
#'   # latent variables
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#' "
#' fs_dat2 <- get_fs(PoliticalDemocracy, model = mod2, std.lv = TRUE)
#' tspa(model = "dem60 ~ ind60
#'               dem65 ~ ind60 + dem60",
#'      data = fs_dat2,
#'      vc = attr(fs_dat2, "av_efs"),
#'      cross_loadings = attr(fs_dat2, "fsA"))
#'
#' # multigroup, two-factor example
#' mod3 <- "
#'   # latent variables
#'     visual =~ x1 + x2 + x3
#'     speed =~ x7 + x8 + x9
#' "
#' fs_dat3 <- get_fs(HolzingerSwineford1939, model = mod3, std.lv = TRUE,
#'                   group = "school")
#' tspa(model = "visual ~ speed",
#'      data = fs_dat3,
#'      vc = attr(fs_dat3, "av_efs"),
#'      cross_loadings = attr(fs_dat3, "fsA"),
#'      group = "school")
#'
#' # multigroup, three-factor example
#' mod4 <- "
#'   # latent variables
#'     visual =~ x1 + x2 + x3
#'     textual =~ x4 + x5 + x6
#'     speed =~ x7 + x8 + x9
#'
#' "
#' fs_dat4 <- get_fs(HolzingerSwineford1939, model = mod4, std.lv = TRUE,
#'                   group = "school")
#' tspa(model = "visual ~ speed
#'               textual ~ visual + speed",
#'      data = fs_dat4,
#'      vc = attr(fs_dat4, "av_efs"),
#'      cross_loadings = attr(fs_dat4, "fsA"),
#'      group = "school")
#'
#' # get factor scores
#' fs_dat_visual <- get_fs(data = HolzingerSwineford1939,
#'                         model = "visual =~ x1 + x2 + x3",
#'                         group = "school")
#' fs_dat_speed <- get_fs(data = HolzingerSwineford1939,
#'                        model = "speed =~ x7 + x8 + x9",
#'                        group = "school")
#' fs_hs <- cbind(do.call(rbind, fs_dat_visual),
#'                do.call(rbind, fs_dat_speed))
#'
#' # tspa model
#' tspa(model = "visual ~ speed",
#'      data = fs_hs,
#'      se = data.frame(visual = c(0.3391326, 0.311828),
#'                      speed = c(0.2786875, 0.2740507)),
#'      group = "school",
#'      group.equal = "regressions")
#'
#' # manually adding equality constraints on the regression coefficients
#' tspa(model = "visual ~ c(b1, b1) * speed",
#'      data = fs_hs,
#'      se = list(visual = c(0.3391326, 0.311828),
#'                speed = c(0.2786875, 0.2740507)),
#'      group = "school")


tspa <- function(model, data, reliability = NULL, se = NULL,
                 vc = NULL, cross_loadings = NULL, ...) {
  if (!is.null(reliability)) {
    stop("tspa() currently does not support reliability model")
  }
  if (!is.data.frame(se)) {
    se <- as.data.frame(as.list(se))
  }
  multigroup <- nrow(se) == 1 | is.list(vc)

  if (multigroup) {
    if (is.null(vc)) { # SE
      tspaModel <- tspaMultipleGroupSe(model, data, se)
    } else { # covariance
      tspaModel <- tspaMultipleGroupMF(model, data, vc, cross_loadings)
      data <- do.call(rbind, data)
    }
  } else {
    if (is.null(vc)) { # SE
      tspaModel <- tspaSingleGroup(model, data, se)
    } else { # covariance
      tspaModel <- tspaSingleGroupMF(model, data, vc, cross_loadings)
    }
  }

  tspa_fit <- sem(model = tspaModel,
                  data  = data,
                  ...)
  # to access the attribute, use attr(x,"tspaModel")
  attr(tspa_fit, "tspaModel") <- tspaModel
  return(tspa_fit)
}

tspaSingleGroup <- function(model, data, se = NULL) {
  if (nrow(se) != 0) {
    ev <- se^2
    var <- names(se)
    len <- length(se)

    # col <- colnames(data)
    fs <- paste0("fs_", var)
    latent_var <- rep(NA, len)
    error_constraint <- rep(NA, len)
    # latent_variance <- rep(NA, len)

    for (x in seq_len(len)) {
      latent_var[x] <- paste0(var[x], " =~ ", fs[x], "\n")
      error_constraint[x] <- paste0(fs[x], " ~~ ", ev[x], " * ", fs[x], "\n")
      # latent_variance[x] <- paste0(var[x], " ~~ v", x, " * ", var[x], "\n")
    }

    latent_var_str <- paste(latent_var, collapse = "")
    error_constraint_str <- paste(error_constraint, collapse = "")
    # latent_variance_str <- paste(latent_variance, collapse="")
    tspaModel <- paste0("# latent variables (indicated by factor scores)\n",
                        latent_var_str,
                        "# constrain the errors\n",
                        error_constraint_str,
                        "# latent variances\n",
                        # latent_variance_str,
                        # "# regressions\n",
                        model,
                        "\n")

    return(tspaModel)
  }
}

tspaSingleGroupMF <- function(model, data, vc, cross_loadings) {
  # ev <- se^2
  var <- colnames(vc)
  len <- nrow(vc)

  col <- colnames(data)
  fs <- paste0("fs_", var)
  colnames(vc) <- rownames(vc) <- fs

  # latent variables
  loadings <- paste0(cross_loadings, " * ", fs)
  loadings_list <- split(loadings, factor(rep(var, each = len),
                                          levels = var))
  loadings_c <- lapply(loadings_list, function(x) {
    paste0(x, collapse = " + ")
  })
  latent_var_str <- paste(var, "=~", loadings_c)
  # error variances
  vc_in <- !upper.tri(vc)
  ev_rhs <- colnames(vc)[col(vc_in)[vc_in]]
  ev_lhs <- rownames(vc)[row(vc_in)[vc_in]]
  error_constraint_str <- paste0(ev_lhs, " ~~ ", vc[vc_in], " * ", ev_rhs)
  # # latent variances
  # latent_variance_str <- paste(var, "~~", var)

  tspaModel <- paste0(c(
    "# latent variables (indicated by factor scores)",
    latent_var_str,
    "# constrain the errors",
    error_constraint_str,
    # "# latent variances",
    # latent_variance_str,
    "# regressions",
    model
  ),
  collpase = "\n")

  return(tspaModel)
}

tspaMultipleGroupMF <- function(model, data, vc, cross_loadings) {
  ngroup <- length(vc)
  var <- colnames(vc[[1]])
  nvar <- length(var)

  col <- colnames(data[[1]])
  fs <- paste0("fs_", var)
  # colnames(vc) <- rownames(vc) <- fs

  # latent variables
  loadings_mat <- matrix(unlist(cross_loadings), ncol = ngroup)
  loadings <- apply(loadings_mat, 1, function(x) {
    paste0("c(", paste0(x, collapse = ", "), ") * ")
  }) |>
    paste0(fs)
  loadings_list <- split(loadings, factor(rep(var, each = nvar),
                                          levels = var))
  loadings_c <- lapply(loadings_list, function(x) {
    paste0(x, collapse = " + ")
  })
  latent_var_str <- paste(var, "=~", loadings_c)
  # error variances
  vc_in <- !upper.tri(vc[[1]])
  ev_rhs <- paste0("fs_", colnames(vc[[1]])[col(vc_in)[vc_in]])
  ev_lhs <- paste0("fs_", rownames(vc[[1]])[row(vc_in)[vc_in]])
  errors_mat <- matrix(unlist(vc), ncol = ngroup)[as.vector(vc_in), ]
  errors <- apply(errors_mat, 1, function(x) {
    paste0("c(", paste0(x, collapse = ", "), ")")
  })
  error_constraint_str <- paste0(ev_lhs, " ~~ ", errors, " * ", ev_rhs)
  # # latent variances
  # latent_variance_str <- paste(var, "~~", var)

  tspaModel <- paste0(c(
    "# latent variables (indicated by factor scores)",
    latent_var_str,
    "# constrain the errors",
    error_constraint_str,
    # "# latent variances",
    # latent_variance_str,
    "# regressions",
    model
  ),
  collpase = "\n")

  return(tspaModel)
}


tspaMultipleGroupSe <- function(model, data, se = NULL) {
  # if (is.list(se)) {
  #   len <-
  # }
  if (nrow(se) != 0) {
    ev <- se^2
    var <- names(se)
    len <- length(se)
    group <- nrow(se)
    # col <- colnames(data)
    fs <- paste0("fs_", var)
    tspaModel <- rep(NA, len)
    latent_var <- rep(NA, len)
    error_constraint <- rep(NA, len)
    # latent_variance <- rep(NA, len)

    for (x in seq_len(len)) {
      latent_var[x] <- paste0(
        var[x], "=~ c(",
        paste0(rep(1, group), collapse = ", "), ") * ", fs[x], "\n"
      )
      error_constraint[x] <- paste0(
        fs[x], "~~ c(", paste(ev[x], collapse = ", "), ") * ", fs[x], "\n"
      )
      # latent_variance[x] <- paste0(var[x], " ~~ c(", paste0(paste0(rep(paste0("v",x), group), 1:group), collapse = ", "), ") * ", var[x], "\n")
    }

    latent_var_str <- paste(latent_var, collapse = "")
    error_constraint_str <- paste(error_constraint, collapse = "")
    # latent_variance_str <- paste(latent_variance, collapse="")
    tspaModel <- paste0("# latent variables (indicated by factor scores)\n",
                        latent_var_str,
                        "# constrain the errors\n",
                        error_constraint_str,
                        # "# latent variances\n",
                        # latent_variance_str,
                        "# regressions\n",
                        model,
                        "\n")

    return(tspaModel)
  }
}
