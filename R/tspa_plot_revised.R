#' Diagnostic plots of fitted 2S-PA model
#'
#' @param tspa_fit An object of class \code{lavaan},
#'                 representing the output generated from `tspa()` function.
#' @param ask Logic input. If 'TRUE' is indicated, the user will be asked before
#'            before each plot is generated. The default setting is 'False'.
#' @param title Character. Set the name of scatter plot. The default value is "Scatterplot".
#' @param label_x Character. Set the  name of the x-axis. The default value is "fs_" followed by variable names.
#' @param label_y Character. Set the  name of the y-axis. The default value is "fs_" followed by variable names.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}. See
#'            \code{\link[graphics]{plot}} for a list.
#' @param abbreviation Logic input. If 'FALSE' is indicated, the group name will be shown in full.
#'                     The default setting is 'True'.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}. See
#'            \code{\link[graphics]{plot}} for a list.
#'
#' @return A scatterplot between factor scores, and a residual plot.
#'
#' @export
#'
#' @examples
#' library(lavaan)
#' model <- '
#' # latent variable definitions
#' ind60 =~ x1 + x2 + x3
#' dem60 =~ y1 + a*y2 + b*y3 + c*y4
#' # regressions
#' dem60 ~ ind60
#' '
#' fs_dat_ind60 <- get_fs(data = PoliticalDemocracy,
#'                        model = "ind60 =~ x1 + x2 + x3")
#' fs_dat_dem60 <- get_fs(data = PoliticalDemocracy,
#'                        model = "dem60 =~ y1 + y2 + y3 + y4")
#' fs_dat <- cbind(fs_dat_ind60, fs_dat_dem60)
#'
#' tspa_fit <- tspa(model = "dem60 ~ ind60",
#'                  data = fs_dat,
#' se = list(ind60 = 0.1213615, dem60 = 0.6756472))
#' tspa_plot(tspa_fit)

tspa_plot <- function(tspa_fit,
                      title = NULL,
                      label_x = NULL,
                      label_y = NULL,
                      abbreviation = TRUE,
                      fscore_type = NULL,
                      ask = FALSE,
                      ...) {

  fit_pars <- lavaan::parameterestimates(tspa_fit) # parameter estimates
  regression_fit <- fit_pars[which(fit_pars$op == "~"),] # path coefficients
  latent_dv <- unique(c(unname(t(regression_fit["lhs"])))) # Extract DV
  latent_iv <- unique(c(unname(t(regression_fit["rhs"])))) # Extract IV
  fscores_func <- lavaan::lavInspect(tspa_fit, what = "data") # factor scores from `get_fs`
  fscores_est <- lavaan::lavPredict(tspa_fit) # factor scores from estimation using `lavPredict`

    # Type of scatterplot
    if (is.null(fscore_type)) {
      fscores <- fscores_func
    } else {
      fscores <- fscore_est
    }

    # Helper function for scatter plot
    # Comment: put it outside the main function
    plot_scatter <- function (fscores_df, iv, dv,
                              # ab_slope, ab_intercept,
                              g_num = NULL, g_name = NULL, ...) {

      # Ask for next plot
      if (ask == TRUE) {
        invisible(readline(prompt = "Hit <Return> to see next plot: "))
      }

      if (!is.null(g_name)) {
        iv_data <- as.data.frame(unname(fscores_df))[ ,paste0("fs_", iv)]
        dv_data <- as.data.frame(unname(fscores_df))[ ,paste0("fs_", dv)]
      } else {
        iv_data <- fscores_df[ ,paste0("fs_", iv)]
        dv_data <- fscores_df[ ,paste0("fs_", dv)]
      }

      plot(iv_data,
           dv_data,
           ylab = ifelse(is.null(label_y), paste0("fs_", dv),
                         ifelse(length(label_y) > 1, label_y[i], label_y)),
           xlab = ifelse(is.null(label_x), paste0("fs_", iv),
                         ifelse(length(label_x) > 1, label_x[i], label_x)),
           main = ifelse(!is.null(g_name),
                         ifelse(is.null(title),
                                paste0("Scatterplot", " (Group ", g_num, ": ", g_name, ")"),
                                ifelse(length(title) > 1, title[i], title)),
                         ifelse(is.null(title),
                                paste0("Scatterplot"),
                                ifelse(length(title) > 1, title[i], title))),
           pch = 16,
           ...)
      # abline(a = ab_intercept, b = ab_slope, lwd = 2)
    }

    plot_residual <- function (fscores_df, iv, dv,
                               g_num = NULL, g_name = NULL, ...) {

      # Ask for next plot
      if (ask == TRUE) {
        invisible(readline(prompt = "Hit <Return> to see next plot: "))
      }

      if (!is.null(g_name)) {
        iv_data <- as.data.frame(unname(fscores_df))[ ,paste0("fs_", iv)]
        dv_data <- as.data.frame(unname(fscores_df))[ ,paste0("fs_", dv)]
      } else {
        iv_data <- fscores_df[ ,paste0("fs_", iv)]
        dv_data <- fscores_df[ ,paste0("fs_", dv)]
      }

      predicted_y <- lavaan::lavPredictY(tspa_fit, ynames = paste0("fs_", dv), xnames = paste0("fs_", iv))
      if (!is.null(g_name)) {
        resid <- dv_data -  predicted_y[which(names(fscores_df) == predicted_y[ ,2]), ][,1]
      } else {
        resid <- dv_data -  predicted_y
      }

      plot(iv_data,
           resid,
           ylab = paste0("Residuals: ", dv),
           xlab = paste0("Fitted values: ", iv),
           main = ifelse(!is.null(g_name),
                         paste0("Residual Plot: ", " (Group ", g_num, ": ", g_name, ")"),
                         paste0("Residual Plot")),
           pch = 18,
           ...)
    }

    # Examine multi-group sample
    if (is.list(fscores)) {

      # Ask for abbreviation
      g_names <- NULL
      if (abbreviation == TRUE) {
          g_names <- abbreviate(names(fscores))
        } else {
          g_names <- names(fscores)
        }

      # Prepare for abline
      df_latent_scores <- lapply(fscores, data.frame)
      # slope <- coef(tspa_fit)[grepl(apply(expand.grid(latent_dv, latent_iv), 1, paste, collapse="~"),
      #                               names(coef(tspa_fit)))]
      # fs_means <- matrix(unlist(lapply(df_latent_scores, apply, 2, mean, na.rm = T)),
      #                    nrow = length(slope),
      #                    byrow = T)
      # intercept <- fs_means[,2] - fs_means[,1]*slope

      for (g in seq_len(length(g_names))) {

        # Scatterplots
        plot_scatter(fscores_df = df_latent_scores[g],
                     iv = latent_iv,
                     dv = latent_dv,
                     # ab_slope = slope[g],
                     # ab_intercept = intercept[g],
                     g_num = g,
                     g_name = g_names[g],
                     ...)

        # Residual Plots
        plot_residual(fscores_df = df_latent_scores[g],
                       iv = latent_iv,
                       dv = latent_dv,
                       g_num = g,
                       g_name = g_names[g],
                       ...)
      }
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

      reg_pairs <- coef(tspa_fit)[names(coef(tspa_fit)) %in%
                                   apply(expand.grid(latent_dv, latent_iv), 1, paste, collapse ="~")]

      for (i in seq_len(length(reg_pairs))) {

        # Scatterplots
        plot_scatter(fscores_df = fscores,
                     iv = unlist(strsplit(names(reg_pairs[i]), split = "~"))[2],
                     dv = unlist(strsplit(names(reg_pairs[i]), split = "~"))[1],
                     # ab_slope = slope[i],
                     # ab_intercept = intercept[i],
                     g_num = i)

        # Resiudal Plots
        plot_residual(fscores_df = fscores,
                      iv = unlist(strsplit(names(reg_pairs[i]), split = "~"))[2],
                      dv = unlist(strsplit(names(reg_pairs[i]), split = "~"))[1])
      }
    }
}
