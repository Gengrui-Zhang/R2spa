
# R2spa

<!-- badges: start -->

[![R-CMD-check](https://github.com/Gengrui-Zhang/R2spa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Gengrui-Zhang/R2spa/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`R2spa` is a free and open-source R package that performs two-stage path
analysis (2S-PA). With 2S-PA, researchers can perform path analysis by
first obtaining factor scores and then adjusting for measurement errors
using estimates of observation-specific reliability or standard error of
those factor scores. As a viable alternative to SEM, 2S-PA has been
shown to give equally-good estimates as SEM in relatively simple models
and large sample sizes, as well as to give more accurate parameter
estimates, has better control of Type I error rates, and has
substantially less convergence problems in more complex models or small
sample sizes.

## Installation

This package is still in developmental stage and can be installed on
GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("Gengrui-Zhang/R2spa")
```

## Example

``` r
library(lavaan)
library(R2spa)

# Joint model
model <- '
  # latent variable definitions
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4

  # regression
    dem60 ~ ind60
'
```

``` r
# 2S-PA
# Stage 1: Get factor scores and standard errors for each latent construct
fs_dat_ind60 <- get_fs(data = PoliticalDemocracy,
                       model = "ind60 =~ x1 + x2 + x3")
fs_dat_dem60 <- get_fs(data = PoliticalDemocracy,
                       model = "dem60 =~ y1 + y2 + y3 + y4")
fs_dat <- cbind(fs_dat_ind60, fs_dat_dem60)

# get_fs() gives a dataframe with factor scores and standard errors
head(fs_dat)
#>     fs_ind60 fs_ind60_se   fs_dem60 fs_dem60_se
#> 1 -0.5261683   0.1213615 -2.7487224   0.6756472
#> 2  0.1436527   0.1213615 -3.0360803   0.6756472
#> 3  0.7143559   0.1213615  2.6718589   0.6756472
#> 4  1.2399257   0.1213615  2.9936997   0.6756472
#> 5  0.8319080   0.1213615  1.9242932   0.6756472
#> 6  0.2123845   0.1213615  0.9922798   0.6756472
```

``` r
# Stage 2: Perform 2S-PA
tspa_fit <- tspa(
  model = "dem60 ~ ind60",
  data = fs_dat,
  se = list(ind60 = 0.1213615, dem60 = 0.6756472)
)
parameterestimates(tspa_fit)
#>        lhs op      rhs label   est    se     z pvalue ci.lower ci.upper
#> 1    ind60 =~ fs_ind60       1.000 0.000    NA     NA    1.000    1.000
#> 2    dem60 =~ fs_dem60       1.000 0.000    NA     NA    1.000    1.000
#> 3 fs_ind60 ~~ fs_ind60       0.015 0.000    NA     NA    0.015    0.015
#> 4 fs_dem60 ~~ fs_dem60       0.456 0.000    NA     NA    0.456    0.456
#> 5    ind60 ~~    ind60    v1 0.416 0.070 5.914      0    0.278    0.553
#> 6    dem60 ~~    dem60    v2 2.842 0.543 5.235      0    1.778    3.906
#> 7    dem60  ~    ind60       1.329 0.332 4.000      0    0.678    1.981
```

<!-- `devtools::build_readme()` -->
