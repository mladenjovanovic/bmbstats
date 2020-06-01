## code to prepare `vertical_jump_data` dataset goes here
require(tidyverse)

set.seed(1667)

measurement_error <- 0.5 #cm

n_athletes <- 30
SESOI_upper <- 2.5 # cm
SESOI_lower <- -2.5 #cm

vertical_jump_data <- tibble(
  Athlete = paste("Athlete", str_pad(string = seq(1, n_athletes), width = 2, pad = "0")),
  Group = rep(c("Treatment", "Control"), length.out = n_athletes),
  `Squat 1RM` = rnorm(n_athletes, 1.5, 0.35),
  `Pre-test` = rnorm(n_athletes, 45, 5) + rnorm(n_athletes, 0, measurement_error),
  `Change` = if_else(Group == "Treatment",
                     # Treatment effects
                     10 + ((`Squat 1RM` - 1.5) * 10) - `Pre-test` / 30 + rnorm(n_athletes, 0, 1.5),
                     # Control effects
                     -1 + ((`Squat 1RM` - 1.5) * 2.5) + `Pre-test` / 90 + rnorm(n_athletes, 0, 1.5)
  ),
  `Post-test` = `Pre-test` + `Change` + rnorm(n_athletes, 0, measurement_error),
  Magnitude = factor(ifelse(`Change` > SESOI_upper, "Higher", ifelse(`Change` < SESOI_lower, "Lower", "Equivalent")),
                     levels = c("Lower", "Equivalent", "Higher")
  )
)


vertical_jump_data <- vertical_jump_data %>%
  arrange(Group, desc(`Change`)) %>%
  mutate(Group = factor(Group, levels = c("Treatment", "Control"))) %>%
  select(Athlete, `Squat 1RM`, Group, `Pre-test`, `Post-test`, `Change`, Magnitude)

usethis::use_data(vertical_jump_data, overwrite = TRUE)
