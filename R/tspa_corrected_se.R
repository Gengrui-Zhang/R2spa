#' First-order correction of sampling covariance for 2S-PA estimates
#' @param tspa_fit A fitted model from [tspa()].
#' @param vfsLT The sampling covariance matrix of `fsL` and `fsT`, which can be
#'              obtained with [get_fs()] with the argument `vfsLT = TRUE`.
#' @param which_free An optional numeric vector indicating which parameters
#'                   in `fsL` and `fsT` are free. The parameters are ordered
#'                   by the `fsL` matrix and the lower-triangular part of
#'                   `fsT`, by columns. For example, for a two-factor model,
#'                   `fsL` and `fsT` are both 2 x 2 matrices, and the 
#'                   error covariance between the two factor scores (i.e., 
#'                   the [2, 1] element in `fsT`) has an index of 6.
#' @param ... Currently not used.
#' @return A corrected covariance matrix in the same dimension as
#'     `vcov(tspa_fit)`.
#' @export
vcov_corrected <- function(tspa_fit, vfsLT, which_free = NULL, ...) {
    if (is.null(attr(tspa_fit, "fsT"))) {
        stop("corrected vcov requires a tspa model with ",
             "vc and cross-loadings specified.")
    }
    val_fsL <- attr(tspa_fit, "fsL")
    num_ld <- length(val_fsL)
    val_fsT <- attr(tspa_fit, "fsT")
    val_fsLT <- c(val_fsL, val_fsT[lower.tri(val_fsT, diag = TRUE)])
    if (is.null(which_free)) {
        which_free <- seq_along(val_fsLT)
    }
    jac <- lavaan::lav_func_jacobian_complex(
        function(x) {
            par <- val_fsLT
            par[which_free] <- x
            ld <- par[seq_len(num_ld)]
            ev <- lavaan::lav_matrix_lower2full(par[(num_ld + 1):length(par)])
            lavaan::coef(update_tspa(tspa_fit, fsL = ld, fsT = ev))
        },
        x = val_fsLT[which_free],
    )
    vcov(tspa_fit) + jac %*% vfsLT %*% t(jac)
}

update_tspa <- function(tspa_fit, fsL, fsT) {
    call <- attr(tspa_fit, which = "tspa_call")
    if (!missing(fsL)) {
        call$cross_loadings <- eval(call$cross_loadings)
        call$cross_loadings[] <- fsL
    }
    if (!missing(fsT)) {
        call$vc <- eval(call$vc)
        call$vc[] <- fsT
    }
    eval(call)
}