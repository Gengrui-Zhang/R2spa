#' Compute interaction indicators for tspa() function
#' @param dat A data frame containing first-order factor score indiactors with standard error.
#' @param fs_name A vector indicating names of factor scores
#' @param se_fs A vector indicating standard error of factor scores
#' @param loading_fs A vector indicating model-implied loadings of factor scores
#' @param lat_var A vector indicating latent variances
#' @param model An optional string specifying the measurement model
#'              in \code{lavaan} syntax.
#' @return A data frame of product indicators for interaction terms,
#'         with their loadings and standard errors.
#'
#' @importFrom utils combn
#'
#' @export

get_fs_int <- function (dat, fs_name, se_fs, loading_fs,
                        lat_var = rep_len(1, length.out = length(fs_name)),
                        model = NULL) {
  # Check inputs
  if (!is.data.frame(dat)) {
    stop("'dat' must be a data frame.")
  }
  check_inputs(fs_name, "fs_name", names(dat))
  check_inputs(se_fs, "se_fs", names(dat))
  check_inputs(loading_fs, "loading_fs", names(dat))
  if (!is.numeric(lat_var) || length(lat_var) != length(fs_name)) {
    stop("'lat_var' must be the same length as 'fs_name'.")
  }

  # Create fs pairs
  if (is.null(model)) {
    fs_pairs <- combn(fs_name, 2, simplify = FALSE)
  } else {
    fs_pairs <- lapply(unlist(strsplit(model, split = "\\+")),
      FUN = function(pair) {
        pair_nospace <- trimws(pair)
        unlist(strsplit(pair_nospace, split = ":"))
      }
    )
  }

  # Create PI with SE
  dat_pi <- dat
  for (i in seq_along(fs_pairs)) {
    pair <- fs_pairs[[i]]
    name_i <- paste(pair, collapse = ":")
    # Mean-centered factor product score
    dat_pi[[name_i]] <- dat[[pair[1]]] * dat[[pair[2]]] -
      mean(dat[[pair[1]]] * dat[[pair[2]]])
    # Compute se and loading of the product indicator
    name_se_i <- paste0(name_i, "_se")
    name_ld_i <- paste0(name_i, "_ld")
    se_vars <- se_fs[match(pair, table = fs_name)]
    loading_vars <- loading_fs[match(pair, table = fs_name)]
    lat_vars <- lat_var[match(pair, table = fs_name)]
    dat_pi[[name_se_i]] <- sqrt(
      dat[[loading_vars[1]]]^2 * dat[[se_vars[2]]]^2 * lat_vars[1] +
        dat[[loading_vars[2]]]^2 * dat[[se_vars[1]]]^2 * lat_vars[2] +
        dat[[se_vars[1]]]^2 * dat[[se_vars[2]]]^2
    )
    # Also loadings
    dat_pi[[name_ld_i]] <- dat[[loading_vars[1]]] * dat[[loading_vars[2]]]
  }
  return(dat_pi)
}

# Helper function for input check
check_inputs <- function(input, input_name, names_dat) {
  if (!is.character(input)) {
    stop(paste("'", input_name, "' must be a character vector.", sep = ""))
  }
  if (!all(input %in% names_dat)) {
    missing <- input[!input %in% names_dat]
    stop(paste("The following element(s) in '", input_name,
      "' do(es) not match any column name(s) in 'dat': ",
      paste(missing, collapse = ", "),
      sep = ""
    ))
  }
}
