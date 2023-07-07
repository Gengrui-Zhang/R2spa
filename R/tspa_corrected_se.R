vcov_corrected <- function(tspa_fit, vfsLT, ...) {
    if (is.null(attr(tspa_fit, "fsT"))) {
        stop("corrected vcov requires a tspa model with ",
             "vc and cross-loadings specified.")
    }
    num_ld <- length(attr(tspa_fit, "fsL"))
    val_fsT <- attr(tspa_fit, "fsT")
    jac <- lavaan::lav_func_jacobian_complex(
        function(x) {
            ld <- x[seq_len(num_ld)]
            ev <- lavaan::lav_matrix_lower2full(x[(num_ld + 1):length(x)])
            lavaan::coef(update_tspa(tspa_fit, fsL = ld, fsT = ev))
        },
        x = c(attr(tspa_fit, "fsL"),
              val_fsT[lower.tri(val_fsT, diag = TRUE)]),
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