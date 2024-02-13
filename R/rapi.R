processData <- function(data, pairs, indics) {
  data_centered <- data %>%
    # Mean-center indicators
    mutate(across(unname(unlist(indics)), ~ . - mean(.)))

  # Dynamically generate interaction terms based on pairs and indics
  for (i in seq_along(pairs)) {
    pair_name <- unname(unlist(pairs)) # Name of the interaction term
    first_set <- indics[[pair_name[1]]] # First set of indicators for interaction
    second_set <- indics[[pair_name[2]]] # Second set of indicators for interaction

    # Calculate sum scores for each set of indicators
    first_sum <- paste0(pair_name[1], "_sum")
    second_sum <- paste0(pair_name[2], "_sum")
    data_centered <- data_centered %>%
      mutate(!!first_sum := rowSums(select(., all_of(first_set)), na.rm = TRUE),
             !!second_sum := rowSums(select(., all_of(second_set)), na.rm = TRUE))

    # Create the interaction term
    int_name <- paste0(pair_name[1], "_int_", pair_name[2])
    data_centered <- data_centered %>%
      mutate(!!int_name := !!sym(first_sum) * !!sym(second_sum))
  }
  return(data_centered)
}

user_alpha <- function(x) {
  covx <- cov(x)
  p <- ncol(x)
  p / (p - 1) * (1 - sum(diag(covx)) / sum(covx))
}

rapi <- function (model, data) {

  # Parse the model
  pairs <- parseInteractionTerms(model)
  indics <- parseIndicators(model) # This should be a list for multiple pairs

  pairs_count <- length(pairs)

  # Update the data to include interactions
  data_int <- processData(data, pairs, indics)

  # Calculate reliability (alpha)
  first_rel <- user_alpha(data_int[, unlist(indics[1])])
  second_rel <- user_alpha(data_int[, unlist(indics[2])])

  # Update the formula
  lat_names <- c(names(indics), paste0(names(indics), collapse = "_"))
  obs_names <- setdiff(colnames(data_int), colnames(data))

  def_lab <- sub(".*~~\\s*(\\w+)\\*.*", "\\1",
                 trimws(unlist(strsplit(model, split = "\n"))[grep("~~", unlist(strsplit(model, split = "\n")), fixed = TRUE)]))

  fac_cstr <- vector()
  ev_cstr <- vector()
  err_cstr <- vector()
  for(x in 1:length(lat_names)){
    fac_cstr[x] <- paste0(lat_names[x], " =~ ", "1 * ", obs_names[x], "\n")
    ev_cstr[x] <- paste0(obs_names[x], " ~~ ", "ev_", lat_names[x], " * ", obs_names[x], "\n")
    err_cstr[x] <- paste0("ev_", lat_names[x], " == ", "(1 - ", lat_names[x], "_rel) * ",
                          def_lab[x], " / ", lat_names[x], "_rel\n")
  }

  err_cstr[3] <- paste0("ev_", lat_names[3], " == ", "ev_", lat_names[1], " * ",def_lab[2],
                        " + ", "ev_", lat_names[2], " * ", def_lab[1], " + ", "ev_", lat_names[1],
                        " * ", "ev_", lat_names[2])

  fac_cstr <- paste(fac_cstr, collapse = "")
  ev_cstr <- paste(ev_cstr, collapse = "")
  err_cstr <- paste(err_cstr, collapse = "")

  lat_cstr <- paste(trimws(unlist(strsplit(model, split = "\n"))
               [grep("~~", unlist(strsplit(model, split = "\n")), fixed = TRUE)]), collapse = "\n")
  reg_str <- gsub(":", "_", trimws(unlist(strsplit(model, split = "\n"))[grep(paste0(lat_names[1], ":", lat_names[2]),
                                                                    unlist(strsplit(model, split = "\n")))]))
  defbeta_cstr <- paste(trimws(unlist(strsplit(model, split = "\n"))
                                         [grep(":=", unlist(strsplit(model, split = "\n")), fixed = TRUE)]), collapse = "\n")

  model_new <- paste0("# Measurement Model (indicated by sum scores)\n",
                      fac_cstr,
                      "# Error Variance\n",
                      ev_cstr,
                      "# Structural Model\n",
                      reg_str,
                      "\n",
                      "# Latent Variance\n",
                      lat_cstr,
                      "\n",
                      "# Error Constraints\n",
                      err_cstr,
                      "\n",
                      "# Defined Betas\n",
                      defbeta_cstr,
                      "\n")

  model_new <- gsub(paste0(lat_names[1], "_rel"), replacement = first_rel, x = model_new)
  model_new <- gsub(paste0(lat_names[2], "_rel"), replacement = second_rel, x = model_new)

  # Fit the model
  fit_rapi <- sem(model_new, data = data_int)
  return(fit_rapi)
}
