function (data, var1, var2, var3 = NULL, match = TRUE, meanC = TRUE,
          residualC = FALSE, doubleMC = TRUE, namesProd = NULL)
{
  # First mean-centering
  dat1 <- scale(data[, var1], scale = FALSE)
  dat2 <- scale(data[, var2], scale = FALSE)
  datProd <- NULL
  datProd <- matrix(0, nrow(data), 1)
  for (i in 1:length(var1))
    datProd <- data.frame(datProd,
                          matrix(rep(dat1[, i],
                                     length(var2)),
                                 ncol = length(var2)) * dat2)

  datProd <- datProd[, -1]
  datProd <- scale(datProd, scale = FALSE)

  if (is.null(namesProd)) {
    temp <- NULL
    for (i in 1:length(var1)) temp <- c(temp, paste(var1[i],
                                                    var2,
                                                    sep = "."))
    colnames(datProd) <- temp
    } else {
    colnames(datProd) <- namesProd
    }
  data.frame(data, datProd)
}
