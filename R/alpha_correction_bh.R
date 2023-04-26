#' Calculate Benjamini-Hochberg alphas for a list of p-values.
#'
#' This function calculates alphas for a list of p-values and for a given false
#' discovery rate (Q). If Q is not provided, a default value of 0.05 is used.
#' The Benjamini-Hochberg alpha corrected is calculated as:
#' alpha=(i/m)Q, where:
#' i = the individual p-value's rank in the list of p-values,
#' m = the total number of tests, and
#' Q = the false discovery rate.
#'
#' @param p_values A list of p-values.
#' @param Q the false discovery rate. If not provided, a default value of 0.05
#' is used.
#' @param output An option to control the function's output. Valid values are:
#' - print - print the data frame to the console only.
#' - data_frame - return the data_frame only.
#' - both - print the data frame to the console and return it.
#' The default is 'both' so if you do not provide this option the data frame
#' will be printed to the console and returned.
#' @param include_is_significant_column This Boolean option will control if the
#' is significant? column is included in the printed or returned data frame.
#' If TRUE, it will be included. If FALSE, it will be omitted. The default is
#' TRUE so if you do not provide this option the is significant? column will
#' be included.
#' @return A list of alpha values or an error message if the provided p-values
#' are not valid.
#' @importFrom stats setNames
#' @importFrom knitr kable
#' @export
get_alphas_bh <-
  function(p_values,
           Q = 0.05,
           output = "both",
           include_is_significant_column = TRUE) {
    if (is.null(p_values) || length(p_values) == 0) {
      stop("Invalid p-values.")
    }
    if (Q < 0 || Q > 1) {
      stop("Invalid Q: ", Q)
    }
    for (p_value in p_values) {
      if (!is.numeric(p_value) || p_value < 0 || p_value > 1) {
        stop("Invalid p-value: ", p_value)
      }
    }
    if (!(output %in% list("print", "data_frame", "both"))) {
      stop("Invalid output requested: ", output)
    }
    if (!(include_is_significant_column %in% list(TRUE, FALSE))) {
      stop(
        "Invalid option provided for include_is_significant_column: ",
        include_is_significant_column
      )
    }
    size <- length(p_values)
    p_values_map <- setNames(p_values, 1:size)
    sorted_p_values_map <- p_values_map[order(unlist(p_values_map))]
    sorted_p_values = lapply(p_values, sort)
    alphas = list()
    p_value_indexes <- names(sorted_p_values_map)
    triples <- list()
    for (i in 1:size) {
      curr_alpha <- (i / size) * Q
      curr_p_value <- as.double(p_values[as.integer(p_value_indexes[i])])
      alphas = append(alphas, curr_alpha)
      is_significant <- if (curr_p_value < curr_alpha)
        'YES'
      else
        'NO'
      triples[[as.integer(p_value_indexes[i])]] <-
        list(curr_p_value, round(curr_alpha, digits = 3), is_significant)
    }
    df = as.data.frame(do.call(rbind, triples))
    colnames(df) <- c('p-value', 'alpha', 'is significant?')
    if (output == "both") {
      print_output(df, include_is_significant_column)
      return(get_columns(df, include_is_significant_column))
    } else if (output == "print") {
      print_output(df, include_is_significant_column)
    } else {
      return(get_columns(df, include_is_significant_column))
    }
  }


print_output <- function(df, include_is_significant_column) {
  if (include_is_significant_column) {
    print(kable(df))
  } else {
    print(kable(df[1:2]))
  }
}

get_columns <- function(df, include_is_significant_column) {
  if (include_is_significant_column) {
    return(df)
  }
  return(df[1:2])
}
