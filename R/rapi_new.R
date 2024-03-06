# Interaction-branch

#' Reliability Adjusted Product Indicator
#'
#' @param model A string variable describing the structural path model with interaction term(s),
#'              in \code{lavaan} syntax.
#' @param data A data frame containing indicator scores.
#' @return An object of \code{lavaan}.
#' @export

rapi <- function (model, data) {

  # Helper function: processing data
  processData <- function (data, pairs, indics) {
    data_centered <- data %>%
      # Mean-center indicators
      mutate(across(unname(unlist(indics)), ~ . - mean(., na.rm = T)))

    # Dynamically generate interaction terms based on pairs and indics
    for (i in seq_along(pairs)) {
      pair_name <- unname(unlist(pairs)) # Name of the interaction term
      first_set <- indics[[pair_name[1]]] # First set of indicators for interaction
      second_set <- indics[[pair_name[2]]] # Second set of indicators for interaction

      # Calculate sum scores for each set of indicators
      first_mean <- paste0(pair_name[1], "_mean")
      second_mean <- paste0(pair_name[2], "_mean")
      data_centered <- data_centered %>%
        rowwise %>%
        mutate(!!first_mean := mean(c_across(all_of(first_set)), na.rm = TRUE),
               !!second_mean := mean(c_across(all_of(second_set)), na.rm = TRUE)) %>%
        ungroup()

      # Create the interaction term
      int_name <- paste0(pair_name[1], "_int_", pair_name[2])
      data_centered <- data_centered %>%
        mutate(!!int_name := !!sym(first_mean) * !!sym(second_mean))
    }
    return(data_centered)
  }

  # Helper function: cronbach's alpha
  user_alpha <- function (x) {
    covx <- cov(x, use = "complete.obs")
    p <- ncol(x)
    p / (p - 1) * (1 - sum(diag(covx)) / sum(covx))
  }

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
  lat_names <- c(pairs[[1]], paste0(pairs[[1]], collapse = "_"))
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

  # Add Y if it is latent
  if (length(setdiff(names(indics), lat_names)) > 0) {
    dep_name <- setdiff(names(indics), lat_names)
    dep_cstr <- paste0(dep_name, " =~ ", paste(indics[[dep_name]], collapse = " + "), collapse = "")
  } else {
    dep_cstr <- ""
  }

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
                      dep_cstr,
                      "\n# Error Variance\n",
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
  fit_rapi <- sem(model_new, data = data_int, bounds = TRUE)
  return(fit_rapi)
}
