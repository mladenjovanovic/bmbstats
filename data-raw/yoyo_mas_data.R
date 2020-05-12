## code to prepare `yoyo_mas_data` dataset
require(tidyverse)

set.seed(1667)

n_subjects <- 30

yoyo_mas_data <- tibble(
  Athlete = paste(
    "Athlete",
    str_pad(
      string = seq(1, n_subjects),
      width = 2,
      pad = "0"
    )
  ),
  `YoYoIR1` = rnorm(
    n = n_subjects,
    mean = 1224,
    sd = 255
  ),
  `MAS` = 3.6 * (0.456 * `YoYoIR1` / 1000 + 3.617) +
    rnorm(n = length(`YoYoIR1`), 0, 0.2)
)

yoyo_mas_data$YoYoIR1 <- round(yoyo_mas_data$YoYoIR1 / 40) * 40
yoyo_mas_data$MAS <- round(yoyo_mas_data$MAS / 0.5) * 0.5


usethis::use_data(yoyo_mas_data, overwrite = TRUE)
