#' Two-Stage Path Analysis
#' @param model A string variable describing the structural path model.
#' @param data A dataframe containing factor scores.
#' @param reliability A numeric vector representing the reliability indexes
#'   of each latent factor.
#' @example
#' tspa(model = "dem60 ~ ind60", data = fs_dat,
#'      reliability = c(ind60 = 0.9651282, dem60 = 0.9055203))

tspa <- function(model, data, reliability = NULL) {
  reliability <- as.data.frame(reliability)
  len <- nrow(reliability)

  rel_list <- list() # create an empty list for storing the variable names

  for (x in 1:len) {
    rel_list <- c(rel_list, row.names(reliability)[[x]])
  }

  fs <- list() # create an empty list for storing the factor score names

  for (x in 1:len) {
    fs <- c(fs, colnames(data)[x])
  }

  tspaModel <- ''

  for (x in 1:len) {
    tspaModel <- paste0(tspaModel,
                        rel_list[[x]], ' =~ 1 * ', fs[[x]], '\n')
  }

  for (x in 1:len) {
    tspaModel <- paste0(tspaModel,
                        fs[x], ' ~~ ev', x, ' * ', fs[x], '\n')
  }

  for (x in 1:len) {
    tspaModel <- paste0(tspaModel,
                        rel_list[x], ' ~~ v', x, ' * ', rel_list[x], '\n')
  }

  tspaModel <- paste0(tspaModel, model, '\n')

  for (x in 1:len) {
    tspaModel <- paste0(tspaModel,
                        'v', x, ' == ', toString(reliability[x,]), ' / ', toString(1 - reliability[x,]), ' * ev', x, '\n')
  }

  tspa_fit <- sem(model = tspaModel,
                  data  = data)
  return (list(tspa_fit, tspaModel))
}

