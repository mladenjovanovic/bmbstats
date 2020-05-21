## code to prepare `basketball_data` dataset

require(tidyverse)

set.seed(1667)

n_athletes <- 30

basketball_data <- tibble(
  Athlete = paste("Athlete", str_pad(string = seq(1, n_athletes), width = 2, pad = "0")),
  Treatment = rep(c("Basketball", "Control"), length.out = n_athletes),
  `Height_0` = if_else(Treatment == "Basketball", rnorm(n_athletes, 200, 9), rnorm(n_athletes, 180, 10)),
  `Causal Effect` = rnorm(n_athletes, 0, 0.5),
  `Height_1` = `Height_0` + `Causal Effect`,
  `Height` = if_else(Treatment == "Basketball", `Height_1`, `Height_0`)
)

basketball_data <- basketball_data %>%
  arrange(Treatment, desc(`Height`)) %>%
  select(Athlete, Treatment, `Height_0`, `Height_1`, `Height`, `Causal Effect`)

usethis::use_data(basketball_data, overwrite = TRUE)
