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
#' @param abbreviation Logic input. If 'FALSE' is indicated
#' @param title Character. A default or user-defined name or of the title of scatterplot.
#' @param label_x Character. A default or user-defined name of the x-aix of scatterplot.
#' @param label_y Character. A default or user-defined name of the y-aix of scatterplot.
#' @param abbreviation Logic input. If 'False' is indicated
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
                      ask = FALSE,
                      title = NULL,
                      label_x = NULL,
                      label_y = NULL,
                      abbreviation = TRUE,
                      ...) {

  # Check if the model input is a tspa fit
  if (is.null(attributes(tspa_fit)$tspaModel)) {
    stop("tspa_plot() function only supports outputs from tspa()")
  }

  fit_data <- lavaan::parameterestimates(tspa_fit)
  latent_scores <- lavInspect(tspa_fit, what = "data")

  if (is.list(latent_scores)) {
    # latent_names <- colnames(latent_scores[[1]])
    df_latent_scores <- lapply(latent_scores, data.frame)
    ifelse(abbreviation == TRUE,
           g_names <- abbreviate(names(df_latent_scores)),
           g_names <- names(df_latent_scores))
    latent_dv <- c(t(na.omit(fit_data[1:(nrow(fit_data)/length(latent_scores)), ][which(fit_data$op == "~"),]["lhs"])))
    latent_iv <- c(t(na.omit(fit_data[1:(nrow(fit_data)/length(latent_scores)), ][which(fit_data$op == "~"),]["rhs"])))
    latent_model <- list()

    for (g in seq(length(df_latent_scores))) {
      for (i in seq(length(latent_dv))) {
        latent_model[[g]] <- lm(as.numeric(t(df_latent_scores[[g]][paste0("fs_", latent_dv[i])])) ~
                             as.numeric(t(df_latent_scores[[g]][paste0("fs_", latent_iv[i])])),
                           data = df_latent_scores[[g]])

        if (ask == TRUE) {
          invisible(readline(prompt = "Hit <Return> to see next plot: "))
        }

        plot(latent_scores[[g]][ , paste0("fs_", latent_iv[i])],
             latent_scores[[g]][ , paste0("fs_", latent_dv[i])],
             ylab = ifelse(is.null(label_y), paste0("fs_", latent_dv[i]),
                           ifelse(length(label_y) > 1, label_y[i], label_y)),
             xlab = ifelse(is.null(label_x), paste0("fs_", latent_iv[i]),
                           ifelse(length(label_x) > 1, label_x[i], label_x)),
             main = ifelse(is.null(title),
                           paste0("Scatterplot", " (Group ", g, ": ", g_names[g], ")"),
                           ifelse(length(title) > 1, title[i], title)),
             pch = 16,
             ...)
        abline(latent_model[[g]])

        if (ask == TRUE) {
          invisible(readline(prompt = "Hit <Return> to see next plot: "))
        }
        df_latent_scores[[g]]$residuals <- latent_model[[g]]$residuals
        plot(latent_scores[[g]][ ,paste0("fs_", latent_iv[i])],
             df_latent_scores[[g]]$residuals,
             ylab = "Residuals",
             xlab = "Fitted values",
             main = paste0("Residual Plot", " (Group ", g, ": ", g_names[g], ")"),
             pch = 18,
             ...)
        abline(0, 0,
               lty = 3)
        lines(lowess(latent_scores[[g]][ ,paste0("fs_", latent_iv[i])], df_latent_scores[[g]]$residuals),
              col = "red")
      }
    }

  } else {
    # latent_names <- colnames(latent_scores)
    df_latent_scores <- data.frame(latent_scores)
    latent_dv <- c(t(fit_data[which(fit_data$op == "~"), ]["lhs"]))
    latent_iv <- c(t(fit_data[which(fit_data$op == "~"), ]["rhs"]))

    for (i in seq(length(latent_dv))) {
      latent_model <- lm(as.numeric(t(df_latent_scores[paste0("fs_", latent_dv[i])])) ~
                           as.numeric(t(df_latent_scores[paste0("fs_", latent_iv[i])])),
                         data = df_latent_scores)

      if (ask == TRUE) {
        invisible(readline(prompt = "Hit <Return> to see next plot: "))
      }

      plot(latent_scores[ , paste0("fs_", latent_iv[i])],
           latent_scores[ , paste0("fs_", latent_dv[i])],
           ylab = ifelse(is.null(label_y), paste0("fs_", latent_dv[i]),
                         ifelse(length(label_y) > 1, label_y[i], label_y)),
           xlab = ifelse(is.null(label_x), paste0("fs_", latent_iv[i]),
                         ifelse(length(label_x) > 1, label_x[i], label_x)),
           main = ifelse(is.null(title),
                         paste0("Scatterplot"),
                         ifelse(length(title) > 1, title[i], title)),
           pch = 16,
           ...)
      abline(latent_model)

      if (ask == TRUE) {
        invisible(readline(prompt = "Hit <Return> to see next plot: "))
      }
      df_latent_scores$residuals <- latent_model$residuals
      plot(latent_scores[ ,paste0("fs_", latent_iv[i])],
           df_latent_scores$residuals,
           ylab = "Residuals",
           xlab = "Fitted values",
           main = "Residual Plot",
           pch = 18,
           ...)
      abline(0, 0,
             lty = 3)
      lines(lowess(latent_scores[ ,paste0("fs_", latent_iv[i])], df_latent_scores$residuals),
            col = "red")
    }

  }

}



