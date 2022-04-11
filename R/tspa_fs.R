library(lavaan)
mod1 <- " f =~ .7 * y1 + .7 * y2 + .7 * y3 + .7 * y4
           f ~~ 1 * f
           y1 ~~ .5 * y1
           y2 ~~ .5 * y2
           y3 ~~ .5 * y3
           y4 ~~ .5 * y4
           y2 ~ .5 * 1
           y3 ~ -.3 * 1 "
dat1 <- simulateData(mod1, sample.nobs = 500)

# Configural model
config_mod <- " f =~ y1 + y2 + y3 + y4 "
config_fit <- cfa(config_mod,
                  data = dat1,
                  std.lv = TRUE,
                  meanstructure = TRUE
)

# Factor scores
lavPredict(config_fit, method = "Bartlett", se = "TRUE")

# Compute factor scores
# Hand calc (group 1)
covy <- config_fit@implied$cov[[1]]  # implied covariances; Sigma
ginvcovy <- MASS::ginv(covy)  # generalized inverse; inverse when not singular
lam <- lavInspect(config_fit, what = "est")$lambda  # loadings
tlam_invcov <- crossprod(lam, ginvcovy)  # t(lam) %*% ginv(Sigma)
# Center the data: y - implied mean; implied mean = mu
y1c <- t(as.matrix(dat1)) - as.vector(config_fit@implied$mean[[1]])
# Formula for computing factor score: (t(lam) %*% ginv(Sigma))^-1 * t(lam) %*% ginv(Sigma) * (y - mu) + alpha
fs_hand <- as.vector(MASS::ginv(tlam_invcov %*% lam) %*% tlam_invcov %*% y1c) +
  as.vector(lavInspect(config_fit, what = "est")$alpha)
lavPredict(config_fit, method = "Bartlett") - fs_hand

# To do:
# - Compute factor scores with inputs of lambda, mu, y (data), Sigma
compute_fs <- function(lambda, mu, y, Sigma) {
  # ...
  y_matrix <- y.matrix()
  y_ginv <- MASS::ginv(y)
  Sigma_ginv <- MASS::ginv(Sigma)
  lambda_transpose <- t(lambda)
  fs_hand <- MASS::ginv(lambda_transpose * Sigma_ginv) * lambda_transpose * Sigma_ginv * (y - mu) + alpha
  return (fs_hand)
}

