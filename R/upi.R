# Interaction-branch

#' Unconstrained product indicator method
#'
#' @param model A string variable describing the structural path model with interaction term(s),
#'              in \code{lavaan} syntax.
#' @param data A data frame containing indicator scores.
#' @return An object of \code{lavaan}.
#' @export

upi <- function (model, data, mode = "match") {

  pairs <- parseInteractionTerms(model)
  indics <- parseIndicators(model) # This should be a list for multiple pairs
  pairs_count <- length(pairs)

  # Generate product indicators for each interaction pairs
  #############################################################################
  # Helper function 3: Generate the indicator products and update the model
  mode = mode
  product_syntax <- function (model, data, inter_pair, indicators, mode) {
    inter_names <- inter_pair
    vec1 <- unname(unlist(indicators[inter_names][1]))
    vec2 <- unname(unlist(indicators[inter_names][2]))

    if (mode == "match") {
      intNames <- paste0(vec1, vec2)
      model_indProd <- paste0("\n          #Indicator Products",
                              "\n          ",
                              paste(" ", paste(inter_names, collapse = "_int_"), "=~", paste(intNames, collapse = " + ")))
    } else {
      intNames <- paste0(rep(vec1, each = length(vec2)), vec2)
      # Update the model
      ind_1 <- vector()
      ind_syntax_vec <- list()
      ind_2 <- vector()
      ind_syntax_vec_2 <- list()
      count = 0

      for (n in vec1) {
        comb <- combn(c(intNames[grepl(n, intNames)]), 2)
        for (i in seq(unique(comb[1, ]))) {
          ind_1[i] <-  paste0("\n           ",
                              unique(comb[1, ])[i],
                              " ~~ ",
                              paste0("th", which(vec1 == n), "*",
                                     matrix(comb[,c(which(as.vector(comb[1, ])
                                                          == unique(comb[1, ])[i]))], nrow = 2)[2, ],
                                     collapse = " + ")
          )
        }
        ind_syntax_vec[[n]] <- ind_1
        count = count + 1
      }

      for (m in vec2) {
        comb <- combn(c(intNames[grepl(m, intNames)]), 2)
        for (i in seq(unique(comb[1, ]))) {
          ind_2[i] <-  paste0("\n           ",
                              unique(comb[1, ])[i],
                              " ~~ ",
                              paste0("th", which(vec2 == m) + count, "*",
                                     matrix(comb[,c(which(as.vector(comb[1, ])
                                                          == unique(comb[1, ])[i]))], nrow = 2)[2, ],
                                     collapse = " + "))
        }
        ind_syntax_vec_2[[m]] <- ind_2
      }

      model_indProd <- paste0("\n          #Indicator Products",
                              "\n          ",
                              paste(" ", paste(inter_names, collapse = "_int_"), "=~", paste(intNames, collapse = " + ")),
                              "\n          # Residual covariances in which same indicators have same constraint",
                              paste(unlist(ind_syntax_vec), collapse = " "),
                              "\n          ",
                              paste(unlist(ind_syntax_vec_2), collapse = " "),
                              "\n          ")
    }
    return(model_indProd)
  }
  while (pairs_count > 0) {
    ind_syntax <- product_syntax(model,
                                 data,
                                 inter_pair = pairs[[pairs_count]],
                                 indicators = indics,
                                 mode = mode)
    model <- paste(model, ind_syntax)
    pairs_count <- pairs_count - 1
  }
  model <- gsub(":", "_int_", model)
  model <- gsub("_int_=", ":=", model)

  #############################################################################
  # Helper function 3: Generate the updated data with indicator products
  # Generate a new data frame with indicators
  pairs_count <- length(pairs)
  indprod_data <- function (data, inter_pair, indicators, mode = mode) {
    inter_names <- inter_pair
    vec1 <- unname(unlist(indicators[inter_names][1]))
    vec2 <- unname(unlist(indicators[inter_names][2]))

    if (mode == "match") {
      intNames <- paste0(vec1, vec2)
      inter_data <- indProd(data = data,
                            var1 = vec1,
                            var2 = vec2,
                            meanC = TRUE,
                            match = TRUE,
                            doubleMC = TRUE,
                            namesProd = intNames)
    } else {
      intNames <- paste0(rep(vec1, each = length(vec2)), vec2)
      inter_data <- indProd(data = data,
                            var1 = vec1,
                            var2 = vec2,
                            meanC = TRUE,
                            match = FALSE,
                            doubleMC = TRUE,
                            namesProd = intNames)
    }

    inter_data <- inter_data[ , !names(inter_data) %in%
                        names(data)]
    return(inter_data)
  }
  while (pairs_count > 0) {
    interim_data <- indprod_data(data,
                                 pairs[[pairs_count]],
                                 indicators = indics,
                                 mode = mode)
    # Mean-center first-order indicators
    data <- as.data.frame(scale(data, center = TRUE, scale = FALSE))
    data <- cbind(data, interim_data)
    pairs_count <- pairs_count - 1
  }

  #############################################################################

  # Fit the model
  fit_indProd <- sem(model, data = data, bounds = TRUE)
  return(fit_indProd)
}
