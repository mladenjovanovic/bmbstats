## code to prepare `weight_data` dataset
require(tidyverse)

n_athletes <- 20
proportional_bias <- 1.01
fixed_bias <- 1
typical_error <- 0.5

set.seed(6667)

weight_data <- tibble(
  Athlete = paste("Athlete", str_pad(string = seq(1, n_athletes), width = 2, pad = "0")),
  `TS` = rnorm(n = n_athletes, mean = 75, sd = 10),
  `OS 1` = `TS` * proportional_bias + fixed_bias + rnorm(n_athletes, 0, typical_error),
  `OS 2` = `TS` * proportional_bias + fixed_bias + rnorm(n_athletes, 0, typical_error),
  `OS 3` = `TS` * proportional_bias + fixed_bias + rnorm(n_athletes, 0, typical_error),
  `OS 4` = `TS` * proportional_bias + fixed_bias + rnorm(n_athletes, 0, typical_error),
  `OS 5` = `TS` * proportional_bias + fixed_bias + rnorm(n_athletes, 0, typical_error)
)

usethis::use_data(weight_data, overwrite = TRUE)
