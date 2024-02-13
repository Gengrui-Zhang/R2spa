parseIndicators <- function (model) {
  indicators <- function (model) {
    model_elements <- unlist(strsplit(model, "\n"))
    indicator_terms <- unlist(strsplit(gsub(" ", "",
                                            model_elements[grep("=~",
                                                                unlist(strsplit(model, "\n")))]),
                                       split = "=~"))
    indicator_vars <- indicator_terms[grepl("+", indicator_terms, fixed = TRUE) == "FALSE"]
    indicator_items <- indicator_terms[grepl("+", indicator_terms, fixed = TRUE) == "TRUE"]
    indicators <- list()
    for (i in seq(indicator_items)) {
      indicators[[i]] <- unlist(strsplit(indicator_items[[i]], split = "+", fixed = TRUE))
      names(indicators)[i] <- paste0(indicator_vars[[i]])
    }
    return(indicators)
  }
  indics <- indicators(model)
  return(indics)
}
