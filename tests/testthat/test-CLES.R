test_that("CLES returns error", {
  expect_error(CLES())
})

test_that("CLES returns 0.5 for equal a and b vectors", {
  a <- rnorm(100)
  b <- a
  expect_equal(CLES(a, b)[[1]], 0.5)
  expect_equal(CLES(a, b, method = "brute-force")[[1]], 0.5)
})

test_that("CLES return 1 for non-overlapping distributions", {
  a <- 1:100
  b <- 1000:1100
  expect_equal(CLES(a, b)[[1]], 1)
  expect_equal(CLES(a, b, method = "brute-force")[[1]], 1)
})

test_that("CLES return 1 for non-overlapping distributions", {
  b <- 1:100
  a <- 1000:1100
  expect_equal(CLES(a, b)[[1]], 0)
  expect_equal(CLES(a, b, method = "brute-force")[[1]], 0)
})
