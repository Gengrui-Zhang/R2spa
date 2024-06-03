# Artificial data set
set.seed(1230)
dd <- data.frame(
  "a" = rnorm(5),
  "b" = runif(5),
  "w" = 1:5,
  "se_a" = rep(.15, 5),
  "se_b" = rgamma(5, 1, 1),
  "se_w" = rep(.2, 5),
  "ld_a" = 1,
  "ld_b" = c(1, 1, 1, .8, .9),
  "ld_w" = 0.7
)

get_fs_int(dd, fs_name = c("a", "b"), se = c(.15, .20),
           loading = c(1, 1, 1))
