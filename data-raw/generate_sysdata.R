library(dplyr)

eyew_lookup <-
  tibble(dates = seq(as.Date("1899-12-31"), as.Date("2099-12-31"), by = 7)) %>%
  mutate(epiweek = lubridate::epiweek(dates)) %>%
  mutate(epiyear = lubridate::epiyear(dates)) %>%
  mutate(epiyear_epiweek = paste0(epiyear, "-", sprintf("%02d", epiweek))) %>%
  mutate(sunday = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
  select(sunday, epiyear_epiweek)

usethis::use_data(eyew_lookup,
                  internal = TRUE, overwrite = TRUE)
