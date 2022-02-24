tspa <- function(model, data, reliability = NULL) {
  # Expand the syntax

  # Run lavaan::sem()

}

# question: can only retrieve parameter names, but what if c(x=1, y=2) ? how to retrieve "x" and "y"?

# model-building code w/o test on actual data
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

  for (x in 1:len) {
    tspaModel <- paste0(tspaModel,
                        rel_list[x], ' =~ 1 * ', fs[x], '\n')
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
                        'v', x, ' == ', toString(reliability[x]), ' / ', toString(1 - reliability[x]), ' * ev', x, '\n')
  }

  tspa_fit <- sem(model = tspaModel,
                  data  = data)
  return (tspa_fit)
}
