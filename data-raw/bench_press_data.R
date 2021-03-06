## code to prepare `bench_press_data` dataset
require(tidyverse)

set.seed(1667)

n_subjects <- 20

SESOI_lower <- -5 # kg
SESOI_upper <- 5 # kg

measurement_error <- 2.5

bench_press_data <- tibble(
  Athlete = paste(
    "Athlete",
    str_pad(
      string = seq(1, n_subjects),
      width = 2,
      pad = "0"
    )
  ),
  `Pre-test` = rnorm(
    n = n_subjects,
    mean = 100,
    sd = 7.5
  ) +
    rnorm(
      n = n_subjects,
      mean = 0,
      sd = measurement_error
    ),
  `Post-test` = `Pre-test` + rnorm(n = n_subjects, mean = 10, sd = 10) + rnorm(
    n = n_subjects,
    mean = 0,
    sd = measurement_error
  ),
  `Change` = `Post-test` - `Pre-test`,
  Magnitude = ifelse(
    `Change` > SESOI_upper,
    "Higher",
    ifelse(
      `Change` < SESOI_lower,
      "Lower",
      "Equivalent"
    )
  )
)

bench_press_data$Magnitude <- factor(
  bench_press_data$Magnitude,
  levels = c("Lower", "Equivalent", "Higher")
)


usethis::use_data(bench_press_data, overwrite = TRUE)
