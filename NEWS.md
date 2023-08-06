# R2spa 0.0.3

- Add function `tspa_plot()` for bivariate and residual plots (#23)

- `get_fs()` gains argument `corrected_fsT` for computing corrected error estimates (#50)

- New function `vcov_corrected()` for computing corrected SEs (#39)

- New function `get_fs_lavaan()` for computing factor scores and relevant matrices directly from a `lavaan` output (#61)

- Update naming of relevant matrices when computing factor scores:
    * `fsT`: error covariance of factor scores
    * `fsL`: loading matrix of factor scores
    * `fsb`: intercepts of factor scores
    * `scoring_matrix`: weights for computing factor scores from items

- New vignettes for:
    * Corrected error variance of factor scores (#50)
    * Corrected standard errors incorporating uncertainty in measurement parameters of factor scores (#39)
    * Using 2S-PA with EFA scores
    * Using 2S-PA with OpenMx and definition variables (PR #57)
    * Latent interaction with categorical indicators (#27)

# R2spa 0.0.2

- Use `pkgdown` to create website, with GitHub action (#22)

- `get_fs()` now returns a list with multi-group models (#29).

- New function `grandStandardizedSolution()` computes standardized solution based on grand mean and grand SD (#13).

- `tspa()` gains argument `vc` and `cross_loadings`, which is useful for factor scores obtained from multi-factor models (#7). See `vignette("multiple-factors")`. 

# R2spa 0.0.1

- **Work-In-Progress!**

- 0.0.1 version
