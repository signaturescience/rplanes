#######################################################################
## examples
#######################################################################
## load pkgs and generic data prep
library(rplanes)
library(magrittr)
library(fiphde)

hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))

tmp_hosp <-
  hosp %>%
  dplyr::select(date, location, flu.admits) %>%
  dplyr::mutate(date = as.Date(date))


#######################################################################
## evaluate a forecast for plane diff and plane_cover
prepped_observed <- to_signal(tmp_hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")

prepped_forecast <- read_forecast(system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")) %>%
  to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)

prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-10-29")
plane_diff(location = "10", input = prepped_forecast, seed = prepped_seed)
plane_cover(location = "10", input = prepped_forecast, seed = prepped_seed)

## try another date and read directly from FluSight repo
prepped_forecast <- read_forecast("https://raw.githubusercontent.com/signaturescience/Flusight-forecast-data/SigSci-TSENS/data-forecasts/SigSci-TSENS/2022-05-09-SigSci-TSENS.csv") %>%
  to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)

prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-05-07")

plane_diff(location = "10", input = prepped_forecast, seed = prepped_seed)
plane_cover(location = "10", input = prepped_forecast, seed = prepped_seed)

##################################################################################
## evaluate an observed signal for plane diff
prepped_observed <- to_signal(tmp_hosp %>% dplyr::filter(date <= "2023-02-04") , outcome = "flu.admits", type = "observed", resolution = "weeks")
prepped_seed <- plane_seed(prepped_observed, cut_date = "2023-01-28")

plane_diff(location = "10", input = prepped_observed, seed = prepped_seed)

## try two observed weeks at once
prepped_observed <- to_signal(tmp_hosp %>% dplyr::filter(date <= "2023-02-11") , outcome = "flu.admits", type = "observed", resolution = "weeks")
prepped_seed <- plane_seed(prepped_observed, cut_date = "2023-01-28")

plane_diff(location = "10", input = prepped_observed, seed = prepped_seed)

##################################################################################
## try on monthly resolution
hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_monthly.csv", package = "rplanes"))

tmp_hosp <-
  hosp %>%
  dplyr::select(date, location, flu.admits) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::filter(date > as.Date("2022-02-01"))

#######################################################################
## evaluate a forecast for plane diff and plane_cover
prepped_observed <- to_signal(tmp_hosp, outcome = "flu.admits", type = "observed", resolution = "months")

prepped_forecast <- read_forecast(system.file("extdata/forecast/2023-06-01-monthly-TSENS.csv", package = "rplanes")) %>%
  to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4, resolution = "months")

prepped_seed <- plane_seed(prepped_observed, cut_date = "2023-05-01")
plane_diff(location = "10", input = prepped_forecast, seed = prepped_seed)
plane_cover(location = "10", input = prepped_forecast, seed = prepped_seed)
