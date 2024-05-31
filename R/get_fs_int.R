#' Compute interaction indicators for tspa() function
#' @param dat A data frame containing first-order factor score indiactors with standard error.
#' @param fs_name A vector indicating names of factor scores
#' @param se A vector indicating standard error of factor scores
#' @param loading A vector indicating model-implied loadings of factor scores
#' @param model An optional string specifying the measurement model
#'              in \code{lavaan} syntax.
#' @return A data frame of product indicators for interaction terms,
#'         with their loadings and standard errors.
#'
#' @importFrom utils combn
#'
#' @export

get_fs_int <- function (dat, fs_name, se_fs, loading_fs, model = NULL) {

  # Helper function for input check
  check_inputs <- function(input, input_name) {
    if (!is.character(input)) {
      stop(paste("The '", input_name, "' variable must be a character vector.", sep = ""))
    }
    if (!all(input %in% names(dat))) {
      missing <- input[!input %in% names(dat)]
      stop(paste("The following element(s) in '", input_name, "' do(es) not match any column name(s) in 'dat': ", paste(missing, collapse = ", "), sep = ""))
    }
  }

  # Check inputs
  if (!is.data.frame(dat)) {
    stop("The the input for 'dat' must be a data frame.")
  }
  check_inputs(fs_name, "fs_name")
  check_inputs(se_fs, "se_fs")
  check_inputs(loading_fs, "loading_fs")

  # Connect fs and se
  fs_list <- mapply(function(x, y, z) list(name = x, se_fs = y, loading_fs = z),
                    fs_name, se_fs, loading_fs, SIMPLIFY = FALSE)

  # Create fs pairs
  if (is.null(model)) {
    fs_pairs <- combn(fs_name, 2, simplify = FALSE)
  } else {
    elements <- trimws(unlist(strsplit(model, "\\+")))
    fs_pairs <- lapply(strsplit(elements, ":"), function(pair) {
      sapply(pair, function(element) {
        paste0(names(element), element)
      })})
  }

  # Observation-specific check
  check_element <- function(x) {
    if(var(x, na.rm = TRUE) == 0) {
      return(unlist(x[1]))
    } else {
      return(x)
    }
  }

  # Create PI with SE
  dat_pi <- data.frame(matrix(ncol = 2*length(fs_pairs), nrow = nrow(dat)))
  for(i in seq_along(fs_pairs)) {
    pair <- fs_pairs[[i]]
    dat_pi[,2*i-1] <- dat[[pair[1]]]*dat[[pair[2]]] - mean(dat[[pair[1]]]*dat[[pair[2]]])

    par_list <- list(loading_1 = unname(unlist(dat[fs_list[[pair[1]]]$loading])),
                     loading_2 = unname(unlist(dat[fs_list[[pair[2]]]$loading])),
                     se_1 = unname(unlist(dat[fs_list[[pair[1]]]$se])),
                     se_2 = unname(unlist(dat[fs_list[[pair[2]]]$se])))
    par_list <- lapply(par_list, check_element)

    dat_pi[,2*i] <- as.data.frame(matrix(rep(sqrt(par_list$loading_2^2*par_list$se_1^2 +
                                                    par_list$loading_1^2*par_list$se_2^2 +
                                                    par_list$se_1^2*par_list$se_2^2),
                                             length(dat_pi[,i]))))
    colnames(dat_pi)[2*i-1] <- paste(pair[1], pair[2], sep = ".")
    colnames(dat_pi)[2*i] <- paste(pair[1], pair[2], "se", sep = ".")
  }
  return(dat_pi)
}
