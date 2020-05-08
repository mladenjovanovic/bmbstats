test_that("mb_proportions return correct values for exact distributions (unpaired)", {
  a <- 1:100
  b <- 1:100
  result <- data.frame(lower = 0.5, equivalent = 0, higher = 0.5)
  # row.names(result) <- "algebraic"
  expect_equal(mb_proportions(a, b, SESOI_lower = 0, SESOI_upper = 0), result)

  result <- data.frame(lower = 0.495, equivalent = 0.01, higher = 0.495)
  # row.names(result) <- "brute-force"
  expect_equal(mb_proportions(a, b, SESOI_lower = 0, SESOI_upper = 0, method = "brute-force"), result)
})

test_that("mb_proportions return correct values for non-overlapping distributions (paired)", {
  a <- 1:100
  b <- 1001:1100
  result <- data.frame(lower = 0, equivalent = 0, higher = 1)
  # row.names(result) <- "algebraic"
  expect_equal(mb_proportions(a, b, SESOI_lower = 0, SESOI_upper = 0, paired = TRUE), result)

  # row.names(result) <- "brute-force"
  expect_equal(mb_proportions(a, b, SESOI_lower = 0, SESOI_upper = 0, method = "brute-force"), result)
})
