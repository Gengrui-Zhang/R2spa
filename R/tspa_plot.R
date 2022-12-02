#' Diagnostic plots of fitted 2S-PA model
#'
#' @param tppa_fit An object of class \code{lavaan},
#'                 representing the output generated from `tspa()` function.
#'
#' @return A scatterplot and a residual plot between factor scores.
#' @export
#' @examples
# model <- '
# # latent variable definitions
# ind60 =~ x1 + x2 + x3
# dem60 =~ y1 + a*y2 + b*y3 + c*y4
# # regressions
# dem60 ~ ind60
# '
# fs_dat_ind60 <- get_fs(data = PoliticalDemocracy,
# model = "ind60 =~ x1 + x2 + x3")
# fs_dat_dem60 <- get_fs(data = PoliticalDemocracy,
#                        model = "dem60 =~ y1 + y2 + y3 + y4")
# fs_dat <- cbind(fs_dat_ind60, fs_dat_dem60)
#
# tspa_fit <- tspa(model = "dem60 ~ ind60",
# data = fs_dat,
# se = list(ind60 = 0.1213615, dem60 = 0.6756472))
# tspa_plot(tspa_fit)

tspa_plot <- function (tspa_fit, ask = FALSE, ...) {

  fit_data <- parameterestimates(tspa_fit)
  latent_scores <- lavInspect(tspa_fit, what = "data")

  if (is.list(latent_scores)) {
    latent_names <- colnames(latent_scores[[1]])
    df_latent_scores <- lapply(latent_scores, data.frame)
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
        plot(latent_scores[[g]][ ,paste0("fs_", latent_iv[i])],
             latent_scores[[g]][ ,paste0("fs_", latent_dv[i])],
             ylab = paste0("fs_", latent_iv[i]),
             xlab = paste0("fs_", latent_dv[i]),
             main = paste0("Scatterplot", " (group ", g, ")"),
             pch = 16,
             ...)
        abline(latent_model[[g]])

        if (ask == TRUE) {
          invisible(readline(prompt = "Hit <Return> to see next plot: "))
        }
        df_latent_scores[[g]]$residuals <- latent_model[[g]]$residuals
        plot(latent_scores[[g]][ ,paste0("fs_", latent_iv[i])], df_latent_scores[[g]]$residuals,
             ylab = "Residuals",
             xlab = "Fitted values",
             main = paste0("Residual Plot (group ", g, ")"),
             pch = 18,
             ...)
        abline(0, 0,
               lty = 3)
        lines(lowess(latent_scores[[g]][ ,paste0("fs_", latent_iv[i])], df_latent_scores[[g]]$residuals),
              col = "red")
      }
    }

  } else {
    latent_names <- colnames(latent_scores)
    df_latent_scores <- data.frame(latent_scores)
    latent_dv <- c(t(fit_data[which(fit_data$op == "~"),]["lhs"]))
    latent_iv <- c(t(fit_data[which(fit_data$op == "~"),]["rhs"]))

    for (i in seq(length(latent_dv))) {
      latent_model <- lm(as.numeric(t(df_latent_scores[paste0("fs_", latent_dv[i])])) ~
                           as.numeric(t(df_latent_scores[paste0("fs_", latent_iv[i])])),
                         data = df_latent_scores)

      if (ask == TRUE) {
        invisible(readline(prompt = "Hit <Return> to see next plot: "))
      }
      plot(latent_scores[ ,paste0("fs_", latent_iv[i])],
           latent_scores[ ,paste0("fs_", latent_dv[i])],
           ylab = paste0("fs_", latent_dv[i]),
           xlab = paste0("fs_", latent_dv[i]),
           main = paste("Scatterplot"),
           pch = 16,
           ...)
      abline(latent_model)

      if (ask == TRUE) {
        invisible(readline(prompt = "Hit <Return> to see next plot: "))
      }
      df_latent_scores$residuals <- latent_model$residuals
      plot(latent_scores[ ,paste0("fs_", latent_iv[i])], df_latent_scores$residuals,
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

