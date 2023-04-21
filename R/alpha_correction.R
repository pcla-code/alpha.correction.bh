#' Calculate Benjamini-Hochberg alphas for a list of p-values.
#'
#' This function calculates alphas for a list of p-values and for a given false
#' discovery rate (Q). If Q is not provided, a default value of 0.05 is used.
#' The Benjamini-Hochberg alpha corrected is calculated as:
#' alpha=(i/m)Q, where:
#' i = the individual p-value's rank in the list of p-values,
#' m = the total number of tests, and
#' Q = the false discovery rate.
#' The function does not expect the p-values to be sorted but it will return
#' alpha values for the sorted p-values
#' so you are strongly encouraged to sort the p-values before passing them to
#' this function - this will make the returned alpha values more readable.
#'
#' @param p_values A list of p-values.
#' @param Q the false discovery rate. If not provided, a default value of 0.05
#' is used.
#' @return A list of alpha values or an error message if the provided p-values
#' are not valid.
#' @export
get_alphas <- function(p_values, Q=0.05){
  if(is.null(p_values) || class(p_values) == "NULL" || class(p_values) == "logical" || length(p_values) == 0){
    stop("Invalid p-values.")
  }
  if(Q<0 || Q>1) {
    stop("Invalid Q: ", Q)
  }
  for(p_value in p_values) {
    if(!is.numeric(p_value) || p_value<0 || p_value>1) {
      stop("Invalid p-value: ", p_value)
    }
  }
  size <- length(p_values)
  p_values_map <- setNames(p_values, 1:size)
  sorted_p_values_map <- p_values_map[order(unlist(p_values_map))]
  sorted_p_values=lapply(p_values, sort)
  alphas=list()
  p_value_indexes <- names(sorted_p_values_map)
  triples <- list()
  #triples[[1]] <- list('p-value', 'alpha', 'is significant?')
  for(i in 1:size){
    curr_alpha <- (i/size)*Q
    curr_p_value <- p_values[as.integer(p_value_indexes[i])]
    alphas=append(alphas, curr_alpha)
    is_significant <- if(curr_p_value < curr_alpha) 'YES' else 'NO'
    #print(curr_p_value)
    #print(curr_alpha)
    #print(is_significant)
    triples[[as.integer(p_value_indexes[i])]] <- list(curr_p_value, round(curr_alpha, digits=3), is_significant)
  }
  df = as.data.frame(do.call(rbind, triples))
  colnames(df) <- c('p-value', 'alpha', 'is significant?')
  print(kable(df), right=FALSE)
  #return(df)
}
