library(knitr)

test_that("invalid p-values 1", {
  expect_error(get_alphas_bh(list()), "Invalid p-values.")
})

test_that("invalid p-value 2", {
  expect_error(get_alphas_bh(list(1, 2, "3")), "Invalid p-value: 2")
})

test_that("invalid p-value 3", {
  expect_error(get_alphas_bh(list(1, "3")), "Invalid p-value: 3")
})

test_that("invalid p-value 4", {
  expect_error(get_alphas_bh(list(.1, .2, -0.05)), "Invalid p-value: -0.05")
})

test_that("invalid Q 1", {
  expect_error(get_alphas_bh(list(.1, .2), -0.01), "Invalid Q: -0.01")
})

test_that("invalid Q 2", {
  expect_error(get_alphas_bh(list(.1, .2), 1.001), "Invalid Q: 1.001")
})

test_that("invalid option for output", {
  expect_error(get_alphas_bh(list(.1, .2), output = "xyz"),
               "Invalid output requested: xyz")
})

test_that("invalid option for include_is_significant_column", {
  expect_error(
    get_alphas_bh(list(.1, .2), include_is_significant_column = "N/A"),
    "Invalid option provided for include_is_significant_column: N/A"
  )
})

test_that("it should calculate alphas 1", {
  # Given
  # The BH calculation for (0.01, 0.039, 0.08) with Q=0.05:
  # m=3
  # 1: 0.01  <= (1/3)*0.05 (0.017) -> YES (This is the largest k)
  # 2: 0.039 >  (2/3)*0.05 (0.033) -> NO
  # 3: 0.08  >  (3/3)*0.05 (0.050) -> NO

  expected_df <- data.frame(
    "p-value" = c(0.08, 0.01, 0.039),
    "alpha" = c(0.050, 0.017, 0.033),
    "is significant?" = c("NO", "YES", "NO"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # When
  actual_df <- get_alphas_bh(list(0.08, 0.01, 0.039), output = "data_frame")

  # Ensure types are aligned for comparison
  # (The function returns doubles and characters)
  actual_df$`p-value` <- as.numeric(actual_df$`p-value`)
  actual_df$alpha <- as.numeric(actual_df$alpha)

  # Then
  expect_equal(actual_df, expected_df, ignore_attr = TRUE)
})

test_that("it should calculate alphas 2", {
  # Given
  # Input order: 0.08, 0.01, 0.039
  # BH Alphas (sorted): 1/3*0.05=0.017, 2/3*0.05=0.033, 3/3*0.05=0.05
  # P-values (sorted): 0.01 (Rank 1), 0.039 (Rank 2), 0.08 (Rank 3)

  # Step-Up Check:
  # Rank 3: 0.08 > 0.05 (NO)
  # Rank 2: 0.039 > 0.033 (NO)
  # Rank 1: 0.01 <= 0.017 (YES) -> This is the largest k.

  expected_df <- data.frame(
    "p-value" = c(0.08, 0.01, 0.039),
    "alpha" = c(0.050, 0.017, 0.033),
    "is significant?" = c("NO", "YES", "NO"),
    check.names = FALSE,
    # Prevents 'p-value' from becoming 'p.value'
    stringsAsFactors = FALSE   # Ensures strings remain characters
  )

  # When
  actual_df <- get_alphas_bh(list(0.08, 0.01, 0.039), output = "data_frame")

  # Ensure numeric types match (unlisting if necessary)
  actual_df$`p-value` <- as.numeric(actual_df$`p-value`)
  actual_df$alpha <- as.numeric(actual_df$alpha)

  # Then
  # ignore_attr = TRUE is helpful to ignore minor metadata differences
  expect_equal(actual_df, expected_df, ignore_attr = TRUE)
})

test_that("it should calculate alphas 3", {
  # Given
  # For list(0.02, 0.03) with Q=0.05:
  # 0.02 is Rank 1, Alpha = 0.025
  # 0.03 is Rank 2, Alpha = 0.050

  expected_df <- data.frame(
    "p-value" = c(0.02, 0.03),
    "alpha" = c(0.025, 0.050),
    "is significant?" = c("YES", "YES"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # When
  actual_df <- get_alphas_bh(list(0.02, 0.03), output = "data_frame")

  # Ensure types are standard numeric/character for the comparison
  actual_df$`p-value` <- as.numeric(actual_df$`p-value`)
  actual_df$alpha <- as.numeric(actual_df$alpha)
  actual_df$`is significant?` <- as.character(actual_df$`is significant?`)

  # Then
  expect_equal(actual_df, expected_df, ignore_attr = TRUE)
})

test_that("it should correctly rescue p-values using step-up logic", {
  # Given
  # P-values: 0.04, 0.03 | m = 2, Q = 0.05
  # Sorted: 0.03 (Rank 1, Alpha 0.025), 0.04 (Rank 2, Alpha 0.050)
  # Step-up: Since 0.04 <= 0.05, the largest k is 2. Both are significant.

  expected_df <- data.frame(
    "p-value" = c(0.04, 0.03),
    "alpha" = c(0.050, 0.025),
    "is significant?" = c("YES", "YES"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # When
  actual_df <- get_alphas_bh(list(0.04, 0.03), output = "data_frame")

  # Cast types to ensure match
  actual_df$`p-value` <- as.numeric(actual_df$`p-value`)
  actual_df$alpha <- as.numeric(actual_df$alpha)

  # Then
  expect_equal(actual_df, expected_df, ignore_attr = TRUE)
})

test_that("it should print output to console", {
  # Given

  # When

  # Then
  expect_output(get_alphas_bh(list(0.08, 0.01, 0.039), output = "print", .07))
})

test_that("it shouldn't print output to console", {
  # Given

  # When

  # Then
  expect_failure(expect_output(get_alphas_bh(list(0.08, 0.01, 0.039), output = "data_frame", .07)))
})

test_that("it shouldn't include is significant column", {
  # Given
  doubles <- list()
  # We use unlist() here to ensure the expected_df contains doubles, not lists.
  # This matches your function's output format exactly.
  doubles[[1]] <- unlist(list(0.08, 0.07))
  doubles[[2]] <- unlist(list(0.01, 0.023))
  doubles[[3]] <- unlist(list(0.039, 0.047))

  expected_df = as.data.frame(do.call(rbind, doubles))
  colnames(expected_df) <- c('p-value', 'alpha')

  # When
  actual_df <-
    get_alphas_bh(
      list(0.08, 0.01, 0.039),
      output = "data_frame",
      include_is_significant_column = FALSE,
      Q = 0.07
    )

  # Then
  # We use ignore_attr = TRUE so the test passes regardless of row-name metadata
  # while strictly checking that the values and column names are identical.
  expect_equal(actual_df, expected_df, ignore_attr = TRUE)
})

test_that("it should print output to console and return dataframe when 'both' option provided",
          {
            # Given
            triples <- list()
            # Use unlist to ensure columns are atomic vectors (doubles/characters)
            # to match the function's output type exactly.
            triples[[1]] <- unlist(list(0.08, 0.07, 'NO'))
            triples[[2]] <- unlist(list(0.01, 0.023, 'YES'))
            triples[[3]] <- unlist(list(0.039, 0.047, 'YES'))

            expected_df = as.data.frame(do.call(rbind, triples), stringsAsFactors = FALSE)
            colnames(expected_df) <- c('p-value', 'alpha', 'is significant?')

            # Ensure the numeric columns are actually numeric and not character/list
            expected_df$`p-value` <- as.numeric(expected_df$`p-value`)
            expected_df$alpha <- as.numeric(expected_df$alpha)

            # When / Then
            # expect_output checks the console print, actual_df catches the return value
            expect_output(actual_df <-
                            get_alphas_bh(list(0.08, 0.01, 0.039), output = "both", Q = 0.07))

            # Use ignore_attr = TRUE to ignore row-name order differences (e.g. 3 1 2)
            expect_equal(actual_df, expected_df, ignore_attr = TRUE)
          })

test_that("it should print output to console and return dataframe when no option provided",
          {
            # Given
            triples <- list()
            # We unlist each entry to ensure the data frame columns are
            # numeric/character vectors rather than lists.
            triples[[1]] <- unlist(list(0.08, 0.07, 'NO'))
            triples[[2]] <- unlist(list(0.01, 0.023, 'YES'))
            triples[[3]] <- unlist(list(0.039, 0.047, 'YES'))

            # stringsAsFactors = FALSE is vital to keep significance as characters
            expected_df = as.data.frame(do.call(rbind, triples), stringsAsFactors = FALSE)
            colnames(expected_df) <- c('p-value', 'alpha', 'is significant?')

            # Convert columns to match the function's output types (doubles)
            expected_df$`p-value` <- as.numeric(expected_df$`p-value`)
            expected_df$alpha <- as.numeric(expected_df$alpha)

            # When / Then
            # Positionally passing .07 for Q
            expect_output(actual_df <-
                            get_alphas_bh(list(0.08, 0.01, 0.039), .07))

            # ignore_attr = TRUE ignores row name metadata (3 1 2 vs 1 2 3)
            # but strictly checks the values.
            expect_equal(actual_df, expected_df, ignore_attr = TRUE)
          })

test_that("it should print output to console and return dataframe when no option and no Q provided",
          {
            # Given
            triples <- list()
            # Unlisting ensures the expected_df contains atomic vectors (doubles/characters)
            # which matches the function's internal data structure.
            triples[[1]] <- unlist(list(0.08, 0.05, 'NO'))
            triples[[2]] <- unlist(list(0.01, 0.017, 'YES'))
            triples[[3]] <- unlist(list(0.039, 0.033, 'NO'))

            expected_df = as.data.frame(do.call(rbind, triples), stringsAsFactors = FALSE)
            colnames(expected_df) <- c('p-value', 'alpha', 'is significant?')

            # Convert to numeric to match function output types
            expected_df$`p-value` <- as.numeric(expected_df$`p-value`)
            expected_df$alpha <- as.numeric(expected_df$alpha)

            # When / Then
            # Uses default Q = 0.05 and default output = "both"
            expect_output(actual_df <-
                            get_alphas_bh(list(0.08, 0.01, 0.039)))

            # ignore_attr = TRUE avoids failure due to row-index names (3 1 2)
            # while strictly validating the data content.
            expect_equal(actual_df, expected_df, ignore_attr = TRUE)
          })
