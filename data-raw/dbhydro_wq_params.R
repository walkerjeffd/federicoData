library(readr)
library(usethis)

dbhydro_wq_params <- read_csv("data-raw/dbhydro_wq_params.csv", col_types = cols(
  .default = col_character()
))

usethis::use_data(dbhydro_wq_params, internal = TRUE)
