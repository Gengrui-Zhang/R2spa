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