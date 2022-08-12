# R2spa

`R2spa` is a free and open source R package that performs two-stage path analysis (2S-PA). With 2S-PA, researchers can perform path analysis by first obtaining factor scores and then adjusting for measurement errors using estimates of observation-specific reliability or standard error of those factor scores. As a viable alternative to SEM, 2S-PA has been shown to give equally-good estimates as SEM in relatively simple models and large sample sizes, as well as to give more accurate parameter estimates, has better control of Type I error rates, and has substantially less convergence problems in more complex models or small sample sizes. 

This package is still in developmental stage and can be installed on GitHub:

```
remotes::install_github("Gengrui-Zhang/R2spa")
```

## Example

```
library(lavaan)

# Joint model
model <- ' 
  # latent variable definitions
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4

  # regression
    dem60 ~ ind60
'

# 2S-PA

# Get factor scores for each latent construct
fs_dat_ind60 <- get_fs(data = PoliticalDemocracy, 
                       model = "ind60 =~ x1 + x2 + x3")
fs_dat_dem60 <- get_fs(data = PoliticalDemocracy, 
                       model = "dem60 =~ y1 + y2 + y3 + y4")
fs_dat <- cbind(fs_dat_ind60, fs_dat_dem60)

# Perform 2S-PA
tspa_fit <- tspa(model = "dem60 ~ ind60", 
                 data = fs_dat, 
                 se = list(ind60 = 0.1213615, dem60 = 0.6756472))
summary(tspa_fit)
```
