# Interaction-branch

#' A function used to generate factor scores with standard errors for interaction terms
#'
#' @param model A string variable describing the structural path model with interaction term(s),
#'              in \code{lavaan} syntax.
#' @param data A data frame containing indicator scores.
#' @return A data frame with factor scores and standard errors.
#' @export

get_fsint <- function (model, data) {
  # Helper function 1: Parsing the model and extract the interaction pairs
  interpairs <- function (model) {
    str_elements <- gsub(" ", "",
                         unlist(strsplit(unlist(strsplit(model, split = "\n|=~|~")),
                                         split = "+", fixed = TRUE)))
    inter_terms <- as.list(gsub("\n", "", str_elements[grep(":", str_elements)]))
    inter_vars <- list()
    for (i in seq(inter_terms)) {
      terms <- strsplit(inter_terms[[i]], split = ":")
      inter_vars[[i]] <- unlist(terms)
      names(inter_vars)[i] <- paste0("inter_pair_", i)
    }
    return(inter_vars)
  }

  pairs <- interpairs(model)
  pairs_count <- length(pairs)

  # Generate a data frame of factor scores
  updated_data <- data
  column_names <- colnames(updated_data)

  # Append the interaction terms
  get_fs_int <- function (pairs, updated_data) {
    fs_int <- updated_data[ ,paste0("fs_", pairs[[1]][1])]*updated_data[ ,paste0("fs_", pairs[[1]][2])]
    fs_int <- fs_int - mean(fs_int)
    return(fs_int)
  }

  get_fs_int_se <- function (pairs, updated_data) {
    fs_int_se <- sqrt(1*updated_data[ ,paste0("fs_", pairs[[1]][1], "_se")][1]^2 +
                        1*updated_data[ ,paste0("fs_", pairs[[1]][2], "_se")][1]^2 +
                        updated_data[ ,paste0("fs_", pairs[[1]][1], "_se")][1]^2*
                        updated_data[ ,paste0("fs_", pairs[[1]][2], "_se")][1]^2)
    fs_int_se <- matrix(rep(fs_int_se, nrow(updated_data)))
    return(fs_int_se)
  }

  while (pairs_count > 0) {
    updated_data <- cbind(updated_data,
                          get_fs_int(pairs, updated_data),
                          get_fs_int_se(pairs, updated_data))
    colnames(updated_data) <- c(column_names,
                                paste0("fs_", paste(pairs[[pairs_count]], collapse = ".")),
                                paste0("fs_", paste(pairs[[pairs_count]], collapse = "."), "_se"))
    pairs_count <- pairs_count - 1
  }

  updated_data <- updated_data %>%
    select_if(~ !any(is.na(.)))

  return(updated_data)
}
