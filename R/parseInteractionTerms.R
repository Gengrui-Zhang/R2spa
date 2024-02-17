# Interaction-branch

#' Unconstrained product indicator method
#'
#' @param model A string variable describing the structural path model with interaction term(s),
#'              in \code{lavaan} syntax.
#' @return A list of interaction pair
#' @export

parseInteractionTerms <- function(model) {
  str_elements <- gsub(" ", "",
                       unlist(strsplit(unlist(strsplit(model, split = "\n|=~|~")),
                                       split = "+", fixed = TRUE)))
  inter_terms <- gsub("\n", "", str_elements[intersect(grep(":", str_elements), grep(":=", str_elements, invert = T))])

  if (grepl("*", inter_terms, fixed = TRUE)) {
    int_label <- unlist(strsplit(inter_terms, split = "\\*"))[1]
    inter_terms <- as.list(unlist(strsplit(inter_terms, split = "\\*"))[2])
  }

  interpairs <- function (inter_terms) {
    inter_vars <- list()
    for (i in seq(inter_terms)) {
      terms <- strsplit(inter_terms[[i]], split = ":")
      inter_vars[[i]] <- unlist(terms)
      names(inter_vars)[i] <- paste0("inter_pair_", i)
    }
    return(inter_vars)
  }
  pairs <- interpairs(inter_terms)
  return(pairs)
}
