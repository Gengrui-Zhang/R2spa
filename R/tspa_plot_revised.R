tspa_plot <- function(tspa_fit,
                      ask = FALSE,
                      title = NULL,
                      label_x = NULL,
                      label_y = NULL,
                      abbreviation = TRUE,
                      fscore_type = NULL,
                      ...) {

  fit_pars <- lavaan::parameterestimates(tspa_fit) # parameter estimates
  fscores_func <- lavaan::lavInspect(tspa_fit, what = "data") # factor scores from `get_fs`
  fscores_est <- lavaan::lavPredict(tspa_fit) # factor scores from estimation using `lavPredict`

    # Type of scatterplot
    if (is.null(fscore_type)) {
      fscores <- fscores_est
    } else {
      fscores <- fscore_func
    }

    # Helper function for scatter plot
    # Comment: put it outside the main function
    plot_scatter <- function (fscores_df, latent_iv, latent_dv,
                              slope, intercept, g_num, g_name, ...) {

      plot(as.data.frame(unname(fscores_df))[ ,latent_iv],
           as.data.frame(unname(fscores_df))[ ,latent_dv],
           ylab = ifelse(is.null(label_y), paste0("fs_", latent_dv[i]),
                         ifelse(length(label_y) > 1, label_y[i], label_y)),
           xlab = ifelse(is.null(label_x), paste0("fs_", latent_iv[i]),
                         ifelse(length(label_x) > 1, label_x[i], label_x)),
           main = ifelse(is.null(title),
                         paste0("Scatterplot", " (Group ", g_num, ": ", g_name, ")"),
                         ifelse(length(title) > 1, title[i], title)),
           pch = 16,
           ...)

        abline(a = intercept, b = slope, lwd = 2)
    }

    # Examine multi-group sample
    if (is.list(fscores)) {

      df_latent_scores <- lapply(fscores, data.frame)

      if (abbreviation == TRUE) {
          g_names <- abbreviate(names(df_latent_scores))
        } else {
          g_names <- names(df_latent_scores)
        }

      latent_dv <- c(t(na.omit(fit_pars[1:(nrow(fit_pars)/length(df_latent_scores)), ]
                               [which(fit_pars$op == "~"),]["lhs"])))
      latent_iv <- c(t(na.omit(fit_pars[1:(nrow(fit_pars)/length(df_latent_scores)), ]
                               [which(fit_pars$op == "~"),]["rhs"])))

      slope <- as.list(coef(tspa_fit)[grep(paste0(latent_dv, "~", latent_iv), names(coef(tspa_fit)))])
      fs_means <- lapply(df_latent_scores, apply, 2, mean, na.rm = T)
      for (i in seq_len(length(slope))) {
        intercept[i] <- as.data.frame(fs_means[i])[2,] - as.data.frame(slope[i])*as.data.frame(fs_means[i])[1,]
      }

      g_nums <- seq_len(length(g_names))

      for (g in seq_len(length(g_names))) {
        plot_scatter(df_latent_scores[g], latent_iv, latent_dv,
                     slope[[g]], intercept[[g]], g_nums[g], g_names[g])
      }

    } else {

      df_latent_scores <- data.frame(fscores)
      latent_dv <- c(t(fit_pars[which(fit_pars$op == "~"), ]["lhs"]))
      latent_iv <- c(t(fit_pars[which(fit_pars$op == "~"), ]["rhs"]))

    }











}
