library(knitr)

test_that("invalid p-values 1", {
  expect_error(get_alphas_bh(list()), "Invalid p-values.")
})

test_that("invalid p-value 1", {
  expect_error(get_alphas_bh(list(1, 2, "3")), "Invalid p-value: 2")
})

test_that("invalid p-value 2", {
  expect_error(get_alphas_bh(list(1, "3")), "Invalid p-value: 3")
})

test_that("invalid p-value 3", {
  expect_error(get_alphas_bh(list(.1, .2,-0.05)), "Invalid p-value: -0.05")
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

test_that("invalid Q 2", {
  expect_error(get_alphas_bh(list(.1, .2), 1.001), "Invalid Q: 1.001")
})

test_that("it should calculate alphas 1", {
  # Given
  triples <- list()
  triples[[1]] <- list(0.08, 0.05, 'NO')
  triples[[2]] <- list(0.01, 0.017, 'YES')
  triples[[3]] <- list(0.039, 0.033, 'NO')

  expected_df = as.data.frame(do.call(rbind, triples))
  colnames(expected_df) <- c('p-value', 'alpha', 'is significant?')

  # When
  actual_df <-
    get_alphas_bh(list(0.08, 0.01, 0.039), output = "data_frame")

  # Then
  expect_equal(actual_df, expected_df)
})

test_that("it should calculate alphas 2", {
  # Given
  triples <- list()
  triples[[1]] <- list(0.08, 0.07, 'NO')
  triples[[2]] <- list(0.01, 0.023, 'YES')
  triples[[3]] <- list(0.039, 0.047, 'YES')

  expected_df = as.data.frame(do.call(rbind, triples))
  colnames(expected_df) <- c('p-value', 'alpha', 'is significant?')

  # When
  actual_df <-
    get_alphas_bh(list(0.08, 0.01, 0.039), output = "data_frame", .07)

  # Then
  expect_equal(actual_df, expected_df)
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
  doubles[[1]] <- list(0.08, 0.07)
  doubles[[2]] <- list(0.01, 0.023)
  doubles[[3]] <- list(0.039, 0.047)

  expected_df = as.data.frame(do.call(rbind, doubles))
  colnames(expected_df) <- c('p-value', 'alpha')

  # When
  actual_df <-
    get_alphas_bh(
      list(0.08, 0.01, 0.039),
      output = "data_frame",
      include_is_significant_column = FALSE,
      .07
    )

  # Then
  expect_equal(actual_df, expected_df)
})

test_that("it should print output to console and return dataframe when 'both' option provided",
          {
            # Given
            triples <- list()
            triples[[1]] <- list(0.08, 0.07, 'NO')
            triples[[2]] <- list(0.01, 0.023, 'YES')
            triples[[3]] <- list(0.039, 0.047, 'YES')

            expected_df = as.data.frame(do.call(rbind, triples))
            colnames(expected_df) <-
              c('p-value', 'alpha', 'is significant?')

            # When
            # Then
            expect_output(actual_df <-
                            get_alphas_bh(list(0.08, 0.01, 0.039), output = "both", .07))
            expect_equal(actual_df, expected_df)
          })

test_that("it should print output to console and return dataframe when no option provided",
          {
            # Given
            triples <- list()
            triples[[1]] <- list(0.08, 0.07, 'NO')
            triples[[2]] <- list(0.01, 0.023, 'YES')
            triples[[3]] <- list(0.039, 0.047, 'YES')

            expected_df = as.data.frame(do.call(rbind, triples))
            colnames(expected_df) <-
              c('p-value', 'alpha', 'is significant?')

            # When
            # Then
            expect_output(actual_df <-
                            get_alphas_bh(list(0.08, 0.01, 0.039), .07))
            expect_equal(actual_df, expected_df)
          })

test_that("it should print output to console and return dataframe when no option and no Q provided",
          {
            # Given
            triples <- list()
            triples[[1]] <- list(0.08, 0.05, 'NO')
            triples[[2]] <- list(0.01, 0.017, 'YES')
            triples[[3]] <- list(0.039, 0.033, 'NO')

            expected_df = as.data.frame(do.call(rbind, triples))
            colnames(expected_df) <-
              c('p-value', 'alpha', 'is significant?')

            # When
            # Then
            expect_output(actual_df <-
                            get_alphas_bh(list(0.08, 0.01, 0.039)))
            expect_equal(actual_df, expected_df)
          })
