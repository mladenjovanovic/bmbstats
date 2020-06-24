## code to prepare `agreement_data` dataset
require(tidyverse)

n_subjects <- 20

criterion_random <- 0.2
practical_fixed <- 1
practical_proportional <- 1.05
practical_random <- 0.5

set.seed(1667)

agreement_data <- tibble(
  Athlete = paste(
    "Athlete",
    str_pad(
      string = seq(1, n_subjects),
      width = 2,
      pad = "0"
    )
  ),
  True_score = rnorm(n_subjects, 45, 5),
  Criterion_score.trial1 = 0 + True_score * 1 + rnorm(n_subjects, 0, criterion_random),
  Criterion_score.trial2 = 0 + True_score * 1 + rnorm(n_subjects, 0, criterion_random),
  Practical_score.trial1 = practical_fixed + True_score * practical_proportional + rnorm(n_subjects, 0, practical_random),
  Practical_score.trial2 = practical_fixed + True_score * practical_proportional + rnorm(n_subjects, 0, practical_random)
)

usethis::use_data(agreement_data, overwrite = TRUE)
