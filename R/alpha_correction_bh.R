#' Calculate Benjamini-Hochberg alphas for a list of p-values.
#'
#' @param p_values A numeric vector of p-values.
#' @param Q The false discovery rate (default 0.05).
#' @param output "print", "data_frame", or "both".
#' @param include_is_significant_column Boolean to include significance column.
#' @return A data frame containing p-values, alphas, and significance.
get_alphas_bh <- function(p_values,
                          Q = 0.05,
                          output = "both",
                          include_is_significant_column = TRUE) {
  # --- 1. Validation for NULL/Empty ---
  if (is.null(p_values) || length(p_values) == 0) {
    stop("Invalid p-values.")
  }

  # --- 2. Validation for Q (Matches your test error message) ---
  if (Q < 0 || Q > 1) {
    stop("Invalid Q: ", Q)
  }

  # --- 3. Individual P-Value Validation (Matches your test error message) ---
  # We loop through to find the specific offender for the error string
  for (p in p_values) {
    if (!is.numeric(p) || p < 0 || p > 1) {
      stop("Invalid p-value: ", p)
    }
  }

  # --- 4. Validation for Output/Options ---
  if (!(output %in% c("print", "data_frame", "both"))) {
    stop("Invalid output requested: ", output)
  }
  if (!is.logical(include_is_significant_column)) {
    stop(
      "Invalid option provided for include_is_significant_column: ",
      include_is_significant_column
    )
  }

  # --- 5. Core Logic (Step-Up Procedure) ---
  p_vec <- as.numeric(unlist(p_values))
  m <- length(p_vec)
  sort_idx <- order(p_vec)
  sorted_p <- p_vec[sort_idx]

  bh_alphas <- (1:m / m) * Q

  # The "Step-Up" Rule: Find largest k where P(k) <= alpha(k)
  is_below <- sorted_p <= bh_alphas
  max_k <- if (any(is_below))
    max(which(is_below))
  else
    0

  # Everything with rank <= max_k is significant
  sig_vector <- rep("NO", m)
  if (max_k > 0) {
    sig_vector[1:max_k] <- "YES"
  }

  # --- 6. Assemble and Re-order back to original input ---
  df_sorted <- data.frame(
    p_val = sorted_p,
    alpha = round(bh_alphas, 3),
    is_sig = sig_vector,
    stringsAsFactors = FALSE
  )

  # We use order(sort_idx) to map the sorted values back to their original slots
  df <- df_sorted[order(sort_idx), ]
  colnames(df) <- c('p-value', 'alpha', 'is significant?')

  # --- 7. Final Output ---
  final_df <- if (include_is_significant_column)
    df
  else
    df[, 1:2]

  if (output %in% c("both", "print")) {
    # Using kable as requested in your original snippet
    print(knitr::kable(final_df, row.names = FALSE))
  }

  if (output %in% c("both", "data_frame")) {
    return(final_df)
  }
}
