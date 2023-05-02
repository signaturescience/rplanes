#######################################################################
## examples
#######################################################################
## load pkgs and generic data prep
library(rplanes)
library(magrittr)
library(fiphde)

hosp <-
  get_hdgov_hosp(limitcols = TRUE) %>%
  prep_hdgov_hosp(., min_per_week = 0, remove_incomplete = TRUE, trim = list(epiyear = 2022, epiweek = 6))

tmp_hosp <-
  hosp %>%
  dplyr::select(date = week_end, location, flu.admits)

#######################################################################
## evaluate a forecast for plane diff
prepped_observed <- to_signal(tmp_hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")

prepped_forecast <- read_forecast("https://raw.githubusercontent.com/signaturescience/Flusight-forecast-data/SigSci-TSENS/data-forecasts/SigSci-TSENS/2022-05-16-SigSci-TSENS.csv") %>%
  to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)

prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-05-14")
plane_diff(location = "10", input = prepped_forecast, seed = prepped_seed)

## try another date
prepped_forecast <- read_forecast("https://raw.githubusercontent.com/signaturescience/Flusight-forecast-data/SigSci-TSENS/data-forecasts/SigSci-TSENS/2022-05-09-SigSci-TSENS.csv") %>%
  to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)

prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-05-07")

plane_diff(location = "10", input = prepped_forecast, seed = prepped_seed)

##################################################################################
## evaluate an observed signal for plane diff
prepped_observed <- to_signal(tmp_hosp %>% dplyr::filter(date <= "2023-02-04") , outcome = "flu.admits", type = "observed", resolution = "weeks")
prepped_seed <- plane_seed(prepped_observed, cut_date = "2023-01-28")

plane_diff(location = "10", input = prepped_observed, seed = prepped_seed)

## try two observed weeks at once
prepped_observed <- to_signal(tmp_hosp %>% dplyr::filter(date <= "2023-02-11") , outcome = "flu.admits", type = "observed", resolution = "weeks")
prepped_seed <- plane_seed(prepped_observed, cut_date = "2023-01-28")

plane_diff(location = "10", input = prepped_observed, seed = prepped_seed)
