tspa_plot <- function(tspa_fit,
                      ask = FALSE,
                      title = NULL,
                      label_x = NULL,
                      label_y = NULL,
                      abbreviation = TRUE,
                      ...) {

  fit_pars <- lavaan::parameterestimates(tspa_fit) # parameter estimates
  fscores_func <- lavaan::lavInspect(tspa_fit, what = "data") # factor scores from `get_fs`
  fscores_est <- lavaan::lavPredict(tspa_fit) # factor scores from estimation using `lavPredict`

    # determine type of scatterplot
    if (is.null(fscore_type)) {
      fscores <- fscores_est
    } else {
      fscores <- fscore_func
    }

    # determine if this is a multi-group sample
    if (is.list(fs_scores)) {
      df_latent_scores <- lapply(fscores, data.frame)
      ifelse(abbreviation == TRUE,
             g_names <- abbreviate(names(df_latent_scores)),
             g_names <- names(df_latent_scores))
      latent_dv <- c(t(na.omit(fit_pars[1:(nrow(fit_pars)/length(fscores)), ][which(fit_pars$op == "~"),]["lhs"])))
      latent_iv <- c(t(na.omit(fit_pars[1:(nrow(fit_pars)/length(fscores)), ][which(fit_pars$op == "~"),]["rhs"])))
    } else {
      df_latent_scores <- data.frame(fscores)
      latent_dv <- c(t(fit_pars[which(fit_pars$op == "~"), ]["lhs"]))
      latent_iv <- c(t(fit_pars[which(fit_pars$op == "~"), ]["rhs"]))
    }











}
