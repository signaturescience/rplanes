#######################################################################
## example
library(rplanes)
library(magrittr)
library(fiphde)
hosp <-
  get_hdgov_hosp(limitcols = TRUE) %>%
  prep_hdgov_hosp(., min_per_week = 0, remove_incomplete = TRUE, trim = list(epiyear = 2022, epiweek = 6))

tmp_hosp <-
  hosp %>%
  dplyr::select(date = week_start, location, flu.admits)

prepped_observed <- to_signal(tmp_hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")

## TODO: add to_signal logic that actually works for type = "forecast"
prepped_forecast <- read_forecast("https://raw.githubusercontent.com/signaturescience/Flusight-forecast-data/SigSci-TSENS/data-forecasts/SigSci-TSENS/2022-05-16-SigSci-TSENS.csv") %>%
  to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)

# prepped_seed <- plane_seed(prepped_observed, cut_date = NULL)
prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-05-15")

prepped_seed

plane_diff(location = "10", input = prepped_observed, seed = prepped_seed)
plane_diff(location = "56", input = prepped_observed, seed = prepped_seed)
plane_diff(location = "10", input = prepped_forecast, seed = prepped_seed)
