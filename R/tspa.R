#' Two-Stage Path Analysis
#' @param model A string variable describing the structural path model.
#' @param data A dataframe containing factor scores.
#' @param reliability A numeric vector representing the reliability indexes
#'   of each latent factor.
#' @example
#' tspa(model = "dem60 ~ ind60", data = fs_dat,
#'      reliability = c(ind60 = 0.9651282, dem60 = 0.9055203))

tspa <- function(model, data, reliability = NULL) {
  models <- strsplit(model, split = " ~ ")
  x <- models[[1]][2]
  y <- models[[1]][1]
  fs_x <- colnames(data)[1]
  fs_y <- colnames(data)[2]
  tspaModel <- paste0(x, ' =~ 1 * ', fs_x, '\n',
    y, ' =~ 1 * ', fs_y, '\n',
    fs_x, ' ~~ ev1 * ', fs_x, '\n',
    fs_y, ' ~~ ev2 * ', fs_y, '\n',
    x, ' ~~ v1 * ', x, '\n',
    y, ' ~~ v2 * ', y, '\n',
    model, '\n',
    'v1 == ', toString(reliability[1]), ' / ', toString(1 - reliability[1]), ' * ev1\n',
    'v2 == ', toString(reliability[2]), ' / ', toString(1 - reliability[2]), ' * ev2\n')
  tspa_fit <- sem(model = tspaModel,
                  data  = data)
  return (tspa_fit)
}

