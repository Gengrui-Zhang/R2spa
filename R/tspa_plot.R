#' Diagnostic plots of fitted 2S-PA model
#'
#' @param tspa_fit An object of class [lavaan][lavaan-class],
#'                 representing the output generated from the [tspa()]
#'                 function.
#' @param title Character. Set the name of scatter plot. The default value is
#'              "Scatterplot".
#' @param label_x Character. Set the name of the x-axis. The default value is
#'                "fs_" followed by variable names.
#' @param label_y Character. Set the name of the y-axis. The default value is
#'                "fs_" followed by variable names.
#' @param abbreviation Logic input. If `FALSE` is indicated, the group name
#'                     will be shown in full. The default setting is 'True'.
#' @param ask Logic input. If `TRUE` is indicated, the user will be asked before
#'            before each plot is generated. The default setting is 'False'.
#' @param fscore_type Character. Set the type of factor score for input.
#'                    The default setting is using factor score from observed
#'                    data (i.e., output from [get_fs()]). If
#'                    `fscore_type = "est"`, then it will use output from
#'                    [lavaan::lavPredict()].
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}. See
#'            \code{\link[graphics]{plot}} for a list.
#'
#' @return A scatterplot between factor scores, and a residual plot.
#'
#' @export
#'
#' @examples
#' library(lavaan)
#' model <- "
#' # latent variable definitions
#' ind60 =~ x1 + x2 + x3
#' dem60 =~ y1 + a*y2 + b*y3 + c*y4
#' # regressions
#' dem60 ~ ind60
#' "
#' fs_dat_ind60 <- get_fs(
#'   data = PoliticalDemocracy,
#'   model = "ind60 =~ x1 + x2 + x3"
#' )
#' fs_dat_dem60 <- get_fs(
#'   data = PoliticalDemocracy,
#'   model = "dem60 =~ y1 + y2 + y3 + y4"
#' )
#' fs_dat <- cbind(fs_dat_ind60, fs_dat_dem60)
#'
#' tspa_fit <- tspa(
#'   model = "dem60 ~ ind60",
#'   data = fs_dat,
#'   se = list(ind60 = 0.1213615, dem60 = 0.6756472)
#' )
#' tspa_plot(tspa_fit)
tspa_plot <- function(tspa_fit,
                      title = NULL,
                      label_x = NULL,
                      label_y = NULL,
                      abbreviation = TRUE,
                      fscores_type = c("original", "lavaan"),
                      ask = FALSE,
                      ...) {
  fit_pars <- lavaan::parameterestimates(tspa_fit) # parameter estimates
  regression_fit <- fit_pars[which(fit_pars$op == "~"), ] # path coefficients
  latent_dv <- unique(c(unname(t(regression_fit["lhs"])))) # Extract DV
  latent_iv <- unique(c(unname(t(regression_fit["rhs"])))) # Extract IV
  fscores_type <- match.arg(fscores_type) # Specify the type of factor scores

  reg_pairs <- names(coef(tspa_fit)[names(coef(tspa_fit)) %in%
    apply(expand.grid(latent_dv, latent_iv), 1, paste, collapse = "~")])

  # Type of scatterplot
  if (fscores_type == "original") {
    fscores <- lavaan::lavInspect(tspa_fit, what = "data") # factor scores from `get_fs`
  } else {
    fscores <- lavaan::lavPredict(tspa_fit) # factor scores from estimation using `lavPredict`
  }

  # Determine multi-group sample
  if (is.list(fscores)) {
    # Ask for abbreviation
    g_names <- NULL
    if (abbreviation == TRUE) {
      g_names <- abbreviate(names(fscores))
    } else {
      g_names <- names(fscores)
    }

    # Prepare for abline
    fscores <- lapply(fscores, data.frame)
    # slope <- coef(tspa_fit)[grepl(apply(expand.grid(latent_dv, latent_iv), 1, paste, collapse="~"),
    #                               names(coef(tspa_fit)))]
    # fs_means <- matrix(unlist(lapply(df_latent_scores, apply, 2, mean, na.rm = T)),
    #                    nrow = length(slope),
    #                    byrow = T)
    # intercept <- fs_means[,2] - fs_means[,1]*slope

    for (g in seq_len(length(g_names))) {
      for (i in seq_len(length(reg_pairs))) {
        # Ask for next plot
        if (ask == TRUE) {
          invisible(readline(prompt = "Hit <Return> to see next plot: "))
        }

        # Scatterplot
        plot_scatter(
          fscores_df = fscores[g],
          iv = unlist(strsplit(reg_pairs[i], split = "~"))[2],
          dv = unlist(strsplit(reg_pairs[i], split = "~"))[1],
          # ab_slope = slope[g],
          # ab_intercept = intercept[g],
          g_num. = g,
          g_name. = g_names[g],
          title. = title[i],
          label_x. = label_x[i],
          label_y. = label_y[i],
          ...
        )

        # Ask for next plot
        if (ask == TRUE) {
          invisible(readline(prompt = "Hit <Return> to see next plot: "))
        }

        # Residual Plot
        plot_residual(
          fscores_df = fscores[g],
          iv = unlist(strsplit(reg_pairs[i], split = "~"))[2],
          dv = unlist(strsplit(reg_pairs[i], split = "~"))[1],
          tspa_fit. = tspa_fit,
          g_num = g,
          g_name = g_names[g],
          ...
        )
      }
    }

    # Examine single-group example
  } else {
    # Prepare for abline
    # slope <- coef(tspa_fit)[names(coef(tspa_fit)) %in%
    #                           apply(expand.grid(latent_dv, latent_iv), 1, paste, collapse ="~")]
    # fs_means <- apply(fscores, 2, mean, na.rm = T)
    # intercept <- c()
    # for(i in seq_len(length(slope))) {
    #   slope_dv <- unlist(strsplit(names(slope[i]), split = "~"))[1]
    #   slope_iv <- unlist(strsplit(names(slope[i]), split = "~"))[2]
    #   intercept[i] <- fs_means[slope_dv] - fs_means[slope_iv]*slope[i]
    # }
    # names(intercept) <- names(slope)

    for (i in seq_len(length(reg_pairs))) {
      # Ask for next plot
      if (ask == TRUE) {
        invisible(readline(prompt = "Hit <Return> to see next plot: "))
      }

      # Scatterplots
      plot_scatter(
        fscores_df = fscores,
        iv = unlist(strsplit(reg_pairs[i], split = "~"))[2],
        dv = unlist(strsplit(reg_pairs[i], split = "~"))[1],
        # ab_slope = slope[i],
        # ab_intercept = intercept[i],
        g_num. = NULL,
        g_name. = NULL,
        title. = title[i],
        label_x. = label_x[i],
        label_y. = label_y[i],
        ...
      )

      # Ask for next plot
      if (ask == TRUE) {
        invisible(readline(prompt = "Hit <Return> to see next plot: "))
      }

      # Resiudal Plots
      plot_residual(
        fscores_df = fscores,
        iv = unlist(strsplit(reg_pairs[i], split = "~"))[2],
        dv = unlist(strsplit(reg_pairs[i], split = "~"))[1],
        tspa_fit. = tspa_fit,
        g_num = NULL,
        g_name = NULL,
        ...
      )
    }
  }
}

# Helper function for the scatter plot
plot_scatter <- function(fscores_df, iv, dv,
                         # ab_slope, ab_intercept,
                         g_num., g_name.,
                         title., label_x., label_y.,
                         ...) {
  # Group name for multiple-group case
  if (!is.null(g_name.)) {
    iv_data <- as.data.frame(unname(fscores_df))[, paste0("fs_", iv)]
    dv_data <- as.data.frame(unname(fscores_df))[, paste0("fs_", dv)]
  } else {
    iv_data <- fscores_df[, paste0("fs_", iv)]
    dv_data <- fscores_df[, paste0("fs_", dv)]
  }

  # ylab
  ylab <- c()
  if (is.null(label_y.)) {
    ylab <- paste0("fs_", dv)
  } else {
    ylab <- label_y.
  }

  # ylab
  xlab <- c()
  if (is.null(label_x.)) {
    xlab <- paste0("fs_", iv)
  } else {
    xlab <- label_x.
  }

  # title
  title <- c()
  if (is.null(title.) & is.null(g_name.)) {
    title <- paste0("Scatterplot")
  } else if (is.null(title.) & !is.null(g_name.)) {
    title <- paste0("Scatterplot", " (Group ", g_num., ": ", g_name., ")")
  } else if (!is.null(title.) & is.null(g_name.)) {
    title <- paste0(title.)
  } else if (!is.null(title.) & !is.null(g_name.)) {
    title <- paste0(title., " (Group ", g_num., ": ", g_name., ")")
  }

  plot(iv_data,
    dv_data,
    ylab = ylab,
    xlab = xlab,
    main = title,
    pch = 16,
    ...
  )
  # abline(a = ab_intercept, b = ab_slope, lwd = 2)
}

# Helper function for the residual plot
plot_residual <- function(fscores_df, iv, dv,
                          tspa_fit. = tspa_fit,
                          g_num. = g_num, g_name. = g_name,
                          ...) {
  if (!is.null(g_name.)) {
    iv_data <- as.data.frame(unname(fscores_df))[, paste0("fs_", iv)]
    dv_data <- as.data.frame(unname(fscores_df))[, paste0("fs_", dv)]
  } else {
    iv_data <- fscores_df[, paste0("fs_", iv)]
    dv_data <- fscores_df[, paste0("fs_", dv)]
  }

  predicted_y <- lavaan::lavPredictY(tspa_fit., ynames = paste0("fs_", dv), xnames = paste0("fs_", iv))

  if (!is.null(g_name.)) {
    resid <- dv_data - predicted_y[which(names(fscores_df) == predicted_y[, 2]), ][, 1]
  } else {
    resid <- dv_data - predicted_y
  }

  # title
  title <- c()
  if (!is.null(g_name.)) {
    title <- paste0("Residual Plot", " (Group ", g_num., ": ", g_name., ")")
  } else {
    title <- paste0("Residual Plot")
  }

  plot(iv_data,
    resid,
    ylab = paste0("Residuals: ", dv),
    xlab = paste0("Fitted values: ", iv),
    main = title,
    pch = 18,
    ...
  )
}
