## code to prepare `height_data` dataset goes here
require(tidyr)
require(devtools)

set.seed(1667)

n_subjects <- 50

# Generate height data
height_data_wide <- data.frame(
  Male = stats::rnorm(
    n = n_subjects,
    mean = 177.8,
    sd = 10.16
  ),
  Female = stats::rnorm(
    n = n_subjects,
    mean = 165.1,
    sd = 8.89
  )
)

height_data <- tidyr::gather(height_data_wide, key = "Gender", value = "Height")

# Order factors
height_data$Gender <- factor(height_data$Gender,
  levels = c("Male", "Female")
)

usethis::use_data(height_data, overwrite = TRUE)
