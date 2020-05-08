test_that("Cohen's d function works", {
  expect_error(cohens_d(1:10, 3:45, paired = TRUE))

  expect_equal(cohens_d(group_a = c(1:3, NA, 1:3), group_b = c(1:2, NA, NA, 1:3), paired = FALSE, na.rm = TRUE), -0.2300895)
  expect_equal(cohens_d(group_a = c(11:14, NA, 22:24), group_b = c(11:13, NA, NA, 22:24), paired = TRUE, na.rm = TRUE), 0)
})
