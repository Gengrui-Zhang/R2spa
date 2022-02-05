tspa <- function(model, data, reliability = NULL) {
  # Expand the syntax

  # Run lavaan::sem()

}

# model-building code w/o test on actual data
tspa <- function(model, data, reliability = NULL) {
  models <- strsplit(model, split = "~")
  ind <- models[0]
  dem <- models[1]
  ev1 <- data['ev1']
  ev2 <- data['ev2']
  fs_ind <- data['fs_ind']
  fs_dem <- data['fs_dem']
  tspaModel <- ind + ' =~ 1 * ' + fs_ind + '\n' +
    dem + ' =~ 1 * ' + fs_dem + '\n' +
    fs_ind + ' ~~ ' + ev1 + ' * ' + fs_ind + '\n' +
    fs_dem + ' ~~ ' + ev2 + ' * ' + fs_dem + '\n' +
    ind + ' ~~ v1 * ' + ind + '\n' +
    dem + ' ~~ v2 * ' + dem + '\n' +
    model + '\n' +
    'v1 == ' + toString(reliability[0]) + ' / ' + toString(1 - reliability[0]) + ' * ' + ev1 + '\n' +
    'v2 == ' + toString(reliability[1]) + ' / ' + toString(1 - reliability[1]) + ' * ' + ev2 + '\n'
  return (tspaModel)
}
