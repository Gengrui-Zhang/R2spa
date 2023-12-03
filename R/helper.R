#' Create block diagonal matrix
#' @param ... Either multiple matrices or a list of matrices.
#' @export
block_diag <- function(...) {
    if (...length() > 1) {
        x <- list(...)
    } else {
        x <- as.list(...)
    }
    p <- 0
    rn <- cn <- NULL
    for (m in x) {
        if (!is.matrix(m) || ncol(m) != nrow(m)) {
            stop("Input to `block_diag()` must be a list",
                 "or multiple arguments of square matrices.")
        }
        rn <- c(rn, rownames(m))
        cn <- c(cn, colnames(m))
        p <- p + ncol(m)
    }
    if (length(rn) + length(cn) > 0 && (length(rn) != p || length(cn) != p)) {
        stop("Matrices must all have column and row names, ",
             "or all have no names.")
    }
    out <- matrix(0, nrow = p, ncol = p,
                  dimnames = list(rn, cn))
    last_col <- 0
    for (m in x) {
        ind <- last_col + seq_len(nrow(m))
        out[ind, ind] <- m
        last_col <- last_col + ncol(m)
    }
    out
}