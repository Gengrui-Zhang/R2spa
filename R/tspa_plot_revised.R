tspa_plot <- function(tspa_fit,
                      ask = FALSE,
                      title = NULL,
                      label_x = NULL,
                      label_y = NULL,
                      abbreviation = TRUE,
                      fscore_type = NULL,
                      ...) {

  fit_pars <- lavaan::parameterestimates(tspa_fit) # parameter estimates
  regression_fit <- fit_pars[which(fit_pars$op == "~"),] # path coefficients
  latent_dv <- unique(c(unname(t(regression_fit["lhs"])))) # Extract DV
  latent_iv <- unique(c(unname(t(regression_fit["rhs"])))) # Extract IV
  fscores_func <- lavaan::lavInspect(tspa_fit, what = "data") # factor scores from `get_fs`
  fscores_est <- lavaan::lavPredict(tspa_fit) # factor scores from estimation using `lavPredict`
  resid_data <- resid(tspa_fit, "obs") # residual data from tspa fit model

    # Type of scatterplot
    if (is.null(fscore_type)) {
      fscores <- fscores_est
    } else {
      fscores <- fscore_func
    }

    # Helper function for scatter plot
    # Comment: put it outside the main function
    plot_scatter <- function (fscores_df, iv, dv,
                              ab_slope, ab_intercept,
                              g_num = NULL, g_name = NULL, ...) {

      if (!is.null(g_name)) {
        iv_data <- as.data.frame(unname(fscores_df))[ ,iv]
        dv_data <- as.data.frame(unname(fscores_df))[ ,dv]
      } else {
        iv_data <- fscores_df[ ,iv]
        dv_data <- fscores_df[ ,dv]
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
      abline(a = ab_intercept, b = ab_slope, lwd = 2)
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
      slope <- coef(tspa_fit)[grepl(apply(expand.grid(latent_dv, latent_iv), 1, paste, collapse="~"),
                                    names(coef(tspa_fit)))]
      fs_means <- matrix(unlist(lapply(df_latent_scores, apply, 2, mean, na.rm = T)),
                         nrow = length(slope),
                         byrow = T)
      intercept <- fs_means[,2] - fs_means[,1]*slope

      # Scatterplots
      for (g in seq_len(length(g_names))) {
        plot_scatter(fscores_df = df_latent_scores[g],
                     iv = latent_iv,
                     dv = latent_dv,
                     ab_slope = slope[g],
                     ab_intercept = intercept[g],
                     g_num = g,
                     g_name = g_names[g],
                     ...)
      }

    } else {

      # Prepare for abline
      slope <- coef(tspa_fit)[names(coef(tspa_fit)) %in%
                                apply(expand.grid(latent_dv, latent_iv), 1, paste, collapse ="~")]
      fs_means <- apply(fscores, 2, mean, na.rm = T)
      intercept <- c()
      for(i in seq_len(length(slope))) {
        slope_dv <- unlist(strsplit(names(slope[i]), split = "~"))[1]
        slope_iv <- unlist(strsplit(names(slope[i]), split = "~"))[2]
        intercept[i] <- fs_means[slope_dv] - fs_means[slope_iv]*slope[i]
      }
      names(intercept) <- names(slope)

      # Scatterplots
      for (i in seq_len(length(slope))) {
        plot_scatter(fscores_df = fscores,
                     iv = unlist(strsplit(names(slope[i]), split = "~"))[2],
                     dv = unlist(strsplit(names(slope[i]), split = "~"))[1],
                     ab_slope = slope[i],
                     ab_intercept = intercept[i],
                     g_num = i,
                     ...)
      }
    }
}
