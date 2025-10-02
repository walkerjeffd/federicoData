library(readr)
library(usethis)

dbhydro_wq_params <- read_csv("data-raw/dbhydro_wq_params.csv", col_types = cols(
  .default = col_character(),
  param_code = col_double()
))

usgs_params <- read_csv("data-raw/usgs_params.csv", col_types = cols(
  .default = col_character()
))

usethis::use_data(dbhydro_wq_params, usgs_params, internal = TRUE, overwrite = TRUE)
