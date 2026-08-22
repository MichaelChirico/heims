library(data.table)

first_levels <- fread("./data-raw/first_levels.csv")

usethis::use_data(first_levels)
