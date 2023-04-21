test_that("invalid p-values 1", {
  expect_error(get_alphas(list()), "Invalid p-values.")
})

test_that("invalid p-value 1", {
  expect_error(get_alphas(list(1,2,"3")), "Invalid p-value: 2")
})

test_that("invalid p-value 2", {
  expect_error(get_alphas(list(1, "3")), "Invalid p-value: 3")
})

test_that("invalid p-value 3", {
  expect_error(get_alphas(list(.1,.2,-0.05)), "Invalid p-value: -0.05")
})

test_that("invalid Q 1", {
  expect_error(get_alphas(list(.1,.2), -0.01), "Invalid Q: -0.01")
})

test_that("invalid Q 2", {
  expect_error(get_alphas(list(.1,.2),1.001), "Invalid Q: 1.001")
})

test_that("it should calculate alphas 1", {
  expect_equal(lapply(get_alphas(list(0.01,0.08,0.039)), round, 5), lapply(list(0.01666667,0.03333333,0.05), round, 5))
})

test_that("it should calculate alphas 2", {
  expect_equal(lapply(get_alphas(list(0.01,0.08,0.039), .07), round, 5), lapply(list(0.02333333,0.04666667,0.07), round, 5))
})

test_that("it should calculate alphas 3", {
  expect_equal(lapply(get_alphas(list(0.08,0.01,0.039)), round, 5), lapply(list(0.01666667,0.03333333,0.05), round, 5))
})

test_that("it should calculate alphas 4", {
  expect_equal(lapply(get_alphas(list(0.039,0.08,0.01), .07), round, 5), lapply(list(0.02333333,0.04666667,0.07), round, 5))
})
