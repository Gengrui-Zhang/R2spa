int_prod <- function (data, var_1, var_2, prod_name = NULL) {

  # First mean-centering
  dat_1 <- scale(data[, var_1], scale = FALSE)
  dat_2 <- scale(data[, var_2], scale = FALSE)

  # Second mean-centering
  prod_data <- matrix(NA, nrow(data), 1)
  for (i in 1:ncol(dat_1)) {
    for (j in 1:ncol(dat_2))
    prod_data <- data.frame(prod_data, dat_1[ ,i]*dat_2[ ,j])
  }

  prod_data <- prod_data[,-1]
  prod_data_colmeans <- colMeans(prod_data, na.rm = T)
  prod_data <- matrix(prod_data_colmeans,
                      nrow = nrow(data),
                      ncol = length(prod_data_colmeans),
                      byrow = T)


  # Rename
  if (is.null(prod_name)) {
    temp_name <- NULL
    for (i in 1:length(var_1)) {
      temp_name <- c(temp_name,
                     paste(var_1[i],
                           var_2,
                           sep = "_"))
    }
    colnames(prod_data) <- temp_name
  } else {
    colnames(prod_data) <- prod_name
  }

  # Append product indicators to the original data frame
  return(data.frame(data, prod_data))
}

