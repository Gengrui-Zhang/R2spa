rapi <- function (model, data, rel) {

  # Parse the model
  pairs <- parseInteractionTerms(model)
  indics <- parseIndicators(model)

  pairs_count <- length(pairs)

  # Generate the mean centered latent scores
  #############################################################################
  pars <- parameterEstimates(upi(data = data, model = model, mode = "match"))

  sum_lambda <- function (x) sum(pars[match(x, pars$rhs), "est"])
  lambda_sum <- lapply(indics, sum_lambda)
  lambda_constraints <- unname(c(unlist(lambda_sum), prod(unlist(lambda_sum[pairs$inter_pair_1]))))

  pars_vcov <- pars[pars$op == "~~", ]
  pars_vcov <- pars_vcov[pars_vcov$label == "", ]
  sum_evar <- function (x) sum(pars_vcov[match(x, pars_vcov$rhs), "est"])
  evars_sum <- lapply(indics, sum_evar)

  # Update the data frame
  centered_vars <- list()
  int_val <- vector()
  for (n in seq(length(indics))) {
    centered_vars[[n]] <- rowSums(data[, indics[[n]]]) - mean(rowSums(data[, indics[[n]]]), na.rm = T)
  }
  centered_vars <- as.data.frame(do.call(cbind, centered_vars))
  names(centered_vars) <- paste0(names(indics), "_centered")
  centered_pred <- centered_vars[, paste0(pairs$inter_pair_1, "_centered")]
  int_centered <- apply(centered_pred, 1, prod) - mean(apply(centered_pred, 1, prod), na.rm = T)
  df_centered <- cbind(centered_vars, int_centered)
  df_new <- cbind(data, centered_vars, int_centered)

  # Generate the error constraints with different reliability estimates
  rel_var1 <- c()
  rel_var2 <- c()

  if (rel == "alpha") {
    rel_var1 <- psych::alpha(df_new[ ,unlist(indics[pairs$inter_pair_1[1]])])$total["raw_alpha"]
    rel_var2 <- psych::alpha(df_new[ ,unlist(indics[pairs$inter_pair_1[2]])])$total["raw_alpha"]
  } else if (rel == "omega") {
    rel_var1 <- psych::omega(df_new[ ,unlist(indics[pairs$inter_pair_1[1]])])$omega_h
    rel_var2 <- psych::omega(df_new[ ,unlist(indics[pairs$inter_pair_1[2]])])$omega_h
  # } else if (rel == "H") {
  #   rel_var1 <- rel_sum1$coefficientH
  #   rel_var2 <- rel_sum2$coefficientH
  } else if (rel == "GLB") {
    rel_var1 <- psych::glb.fa(df_new[ ,unlist(indics[pairs$inter_pair_1[1]])])$glb
    rel_var2 <- psych::glb.fa(df_new[ ,unlist(indics[pairs$inter_pair_1[2]])])$glb
  }

  evars_int <- rel_var1*var(centered_pred[,1], na.rm = T)*(1 - rel_var2)*var(centered_pred[,2], na.rm = T) +
    rel_var2*var(centered_pred[,2], na.rm = T)*(1 - rel_var1)*var(centered_pred[,1], na.rm = T) +
    (1 - rel_var2)*var(centered_pred[,2], na.rm = T)*(1 - rel_var1)*var(centered_pred[,1], na.rm = T)

  evars_constraints <- unname(c(unlist(evars_sum), evars_int))

  # Update the formula
  names <- c(names(indics), "int")
  latent_var <- vector()
  error_constraint <- vector()
  for(x in 1:length(names)){
    latent_var[x] <- paste0(names[x], " =~ ", lambda_constraints[x], "*", names(df_centered)[x], "\n")
    error_constraint[x] <- paste0(names(df_centered)[x], " ~~ ", evars_constraints[x], "*", names(df_centered)[x], "\n")
  }
  latent_var_str <- paste(latent_var, collapse = "")
  error_constraint_str <- paste(error_constraint, collapse = "")

  reg_str <- gsub(inter_terms[[1]], "int", trimws(unlist(strsplit(model, split = "\n"))
                    [grep(inter_terms[[1]], unlist(strsplit(model, split = "\n")), fixed = TRUE)]))

  var_constraint_str <- paste(trimws(unlist(strsplit(model, split = "\n"))
         [grep("~~", unlist(strsplit(model, split = "\n")), fixed = TRUE)]), collapse = "\n")

  defbeta_constraint_str <- paste(trimws(unlist(strsplit(model, split = "\n"))
                                     [grep(":=", unlist(strsplit(model, split = "\n")), fixed = TRUE)]), collapse = "\n")

  model_new <- paste0("# latent variables (indicated by sum scores)\n",
                                     latent_var_str,
                                     "# constrain the errors\n",
                                     error_constraint_str,
                                     "# regressions\n",
                                     reg_str,
                                     "\n",
                                     "# Variance Constriants\n",
                                     var_constraint_str,
                                     "\n",
                                     "# Defined Betas\n",
                                     defbeta_constraint_str,
                                     "\n")

  # Fit the model
  fit_rapi <- sem(model_new, data = df_new)
  return(fit_rapi)
}
