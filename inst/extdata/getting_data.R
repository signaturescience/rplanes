library(tidyverse)
library(fiphde)
library(here)

# forcast data ####
# dates are "2023-02-06", "2022-10-31"

base_url <- "https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-forecasts/"
## construct full url
forecaster = "SigSci-TSENS"
date = "2022-10-31"
ens <- paste0(base_url, forecaster, "/", date, "-", forecaster, ".csv")
tsens1 = read_csv(ens)

write_csv(tsens1, here("inst/extdata/2022-10-31-SigSci-TSENS.csv"))
write_csv(tsens2, here("inst/extdata/2023-02-06-SigSci-TSENS.csv"))
write_csv(creg1, here("inst/extdata/2022-10-31-SigSci-CREG.csv"))
write_csv(creg2, here("inst/extdata/2023-02-06-SigSci-CREG.csv"))

# observed data ####
# daily observations
hosp_daily <-
  get_hdgov_hosp(limitcols = TRUE) %>%
  filter(date > "2022-04-03" & date < "2023-04-03") %>%
  dplyr::select(date, state, flu.admits)

write_csv(hosp_daily, here("inst/extdata/hdgov_hosp_daily.csv"))

# weekly observations
hosp_week = get_hdgov_hosp(limitcols = TRUE) %>%
  prep_hdgov_hosp(., min_per_week = 0, remove_incomplete = TRUE, trim = list(epiyear = 2022, epiweek = 6))

hosp_week = hosp_week %>%
  dplyr::select(date = week_end, epiyear, epiweek, location, flu.admits)

write_csv(hosp_week, here("inst/extdata/hdgov_hosp_weekly.csv"))

# Quantiles ---------------------------------------------------------------

# quantiles needed
q <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
# Figure out what the interval you need to get those quantiles
qi <-
  tibble(lower=q[q<=.5], upper=rev(q[q>=.5])) %>%
  mutate(interval=as.integer(round((upper-lower)*100)))
qi
# The quidk (say: "quiddick") tibble: QUantile, Interval, Direction, Key
quidk <-
  qi %>%
  gather(direction, quantile, lower, upper) %>%
  mutate(key=paste0(interval, "%_", direction)) %>%
  arrange(quantile) %>%
  select(quantile, interval, direction, key)
quidk

saveRDS(quidk, here("data-raw/quidk.rds"))

# Monthly hosp data ####

hosp <- get_hdgov_hosp(limitcols = TRUE)

hosp_month <- hosp %>%
  dplyr::rename(location = state) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  filter(!year %in% 2019) %>% # remove year 2019 only a few reported
  group_by(location, year, month) %>%
  dplyr::summarize(dplyr::across(c(flu.admits, flu.admits.cov), ~sum(.x, na.rm = TRUE)), .groups = "drop")

locations = readRDS(here("data-raw/locations.rds"))

# generate a US location that is the sum of admits for all locations and add them to the hosp_month
all_hosp = hosp_month %>% dplyr::group_by(year, month) %>%
  dplyr::summarize(dplyr::across(c(flu.admits, flu.admits.cov), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  dplyr::mutate(location = "US", .before = 1) %>% dplyr::bind_rows(hosp_month) %>%
  dplyr::arrange(location, year, month) %>%
  dplyr::rename(abbreviation = location) %>%
  dplyr::inner_join(locations, ., by = "abbreviation") %>%
  dplyr::select(-location_name, -population) %>%
  dplyr::filter(location %in% c("US", stringr::str_pad(1:56, width = 2, pad = "0"))) %>%
  dplyr::filter(abbreviation != "DC") %>%
  dplyr::mutate(date = lubridate::make_date(year, month)) %>%
  dplyr::select(date, year, month, location, flu.admits)

glimpse(all_hosp)


write_csv(all_hosp, here("inst/extdata/hdgov_hosp_monthly.csv"))

# Monthly Forecast ####

# make all_hosp a tsibble. A tsibble is sorted by its key first and index.
hosp_tbl <- all_hosp %>% dplyr::mutate(ymonth = tsibble::make_yearmonth(year = year, month = month), .after = "month") %>%
  tsibble::as_tsibble(index = ymonth, key = location) %>%
  tsibble::fill_gaps()

# Run the fiphde::ts_fit_forecast
hosp_fitfor <- fiphde::ts_fit_forecast(hosp_tbl,
                               horizon=4L,
                               outcome="flu.admits",
                               trim_date = NULL,
                               covariates=FALSE,
                               models=list(arima='PDQ(0, 0, 0) + pdq(1:2, 0:2, 0)',
                                           ets='season(method="N")',
                                           nnetar=NULL),
                               ensemble=TRUE)
#$tsfit
# A mable: 50 x 4
# Key:     location [50]
#location                  arima          ets      ensemble
#<chr>                   <model>      <model>       <model>
# 01       <ARIMA(2,0,0) w/ mean> <ETS(A,N,N)> <COMBINATION>
# 02       <ARIMA(2,0,0) w/ mean> <ETS(A,N,N)> <COMBINATION>
# 04               <ARIMA(2,1,0)> <ETS(A,N,N)> <COMBINATION>
# 06       <ARIMA(2,0,0) w/ mean> <ETS(A,N,N)> <COMBINATION>
# 08               <ARIMA(1,0,0)> <ETS(A,N,N)> <COMBINATION>
# 09               <ARIMA(1,0,0)> <ETS(A,N,N)> <COMBINATION>
# 10       <ARIMA(2,0,0) w/ mean> <ETS(A,N,N)> <COMBINATION>
# 12               <ARIMA(2,1,0)> <ETS(A,N,N)> <COMBINATION>
# 13               <ARIMA(2,1,0)> <ETS(A,N,N)> <COMBINATION>
# 15               <ARIMA(1,1,0)> <ETS(A,N,N)> <COMBINATION>
  # ℹ 40 more rows
  # ℹ Use `print(n = ...)` to see more rows

#$tsfor
# A fable: 600 x 5 [1M]
# Key:     location, .model [150]
#location .model     ymonth    flu.admits .mean
#<chr>    <chr>       <mth>        <dist> <dbl>
# 01       arima    2023 Jun  N(85, 21460)  85.3
# 01       arima    2023 Jul N(119, 45647) 119.
# 01       arima    2023 Aug N(141, 54557) 141.
# 01       arima    2023 Sep N(148, 55189) 148.
# 01       ets      2023 Jun  N(58, 32326)  58.0
# 01       ets      2023 Jul  N(58, 64646)  58.0
# 01       ets      2023 Aug  N(58, 96966)  58.0
# 01       ets      2023 Sep N(58, 129286)  58.0
# 01       ensemble 2023 Jun  N(72, 24027)  71.6
# 01       ensemble 2023 Jul  N(88, 49396)  88.3
# ℹ 590 more rows
# ℹ Use `print(n = ...)` to see more rows

#$formulas
#$formulas$arima
#flu.admits ~ PDQ(0, 0, 0) + pdq(1:2, 0:2, 0)
#<environment: 0x0000021fb387f970>

#$formulas$ets
#flu.admits ~ season(method = "N")
#<environment: 0x0000021fb387f970>


#$nullmodels
# A tibble: 1 × 2
#location model
#<chr>    <chr>
# 05       ets



point_estimates <- hosp_fitfor$tsfor %>% dplyr::as_tibble() %>% dplyr::mutate(quantile = NA_real_, .after = ymonth) %>%
  dplyr::mutate(type = "point") %>%
  dplyr::rename(value = .mean) %>%
  dplyr::select(.model, ymonth, location, quantile, value, type) %>%
  dplyr::arrange(.model, ymonth)

quantiles <- hosp_fitfor$tsfor %>%
  fabletools::hilo(as.double(sort(unique(quidk$interval)))) %>%
  fabletools::unpack_hilo(dplyr::ends_with("%")) %>%
  tidyr::gather(key, value, dplyr::contains("%")) %>%
  dplyr::inner_join(quidk, by = "key") %>%
  tibble::as_tibble() %>%
  dplyr::transmute(.model, ymonth, location, quantile, value, type = "quantile") %>%
  dplyr::arrange(.model, ymonth, quantile) %>%
  dplyr::distinct()

submission_list <- list(point_estimates, quantiles) %>%
  purrr::reduce(dplyr::bind_rows) %>% dplyr::select(.model:type) %>%
  dplyr::arrange(.model, location, type, quantile, ymonth) %>%
  dplyr::group_by(ymonth) %>% dplyr::mutate(N = dplyr::cur_group_id()) %>%
  dplyr::ungroup() %>% dplyr::mutate(target = paste(N, "month ahead inc flu hosp")) %>%
  dplyr::select(-N) %>% dplyr::mutate(target_end_date = lubridate::as_date(ymonth)) %>%
  dplyr::mutate(forecast_date = lubridate::today()) %>%
  dplyr::select(.model, forecast_date, target, target_end_date, location, type, quantile, value) %>%
  dplyr::mutate(value = ifelse(value < 0, 0, value)) %>%
  dplyr::mutate(.counts = TRUE) %>%
  dplyr::mutate(value = ifelse(.counts, ceiling(value), value)) %>%
  dplyr::select(-.counts) %>% dplyr::mutate(quantile = stringr::str_pad(quantile, width = 5, pad = "0", side = "right")) %>%
  dplyr::mutate(value = as.character(value)) %>%
  split(.$.model) %>% purrr::map(dplyr::select, -.model)

# plot the truth/survey data all_hosp with the ensemble forecasts in submission_list

loc <- c("06", "36", "48", "51")

# Grab the real data
real <-
  all_hosp %>%
  tibble::as_tibble() %>%
  dplyr::filter(location %in% loc) %>%
  dplyr::mutate(date = lubridate::make_date(year, month)) %>%
  dplyr::select(location, date, point= flu.admits) %>%
  dplyr::mutate(model="Observed")

# Grab the forecasted data
forecasted <-
  submission_list$ensemble %>%
  dplyr::filter(type == "point") %>%
  dplyr::filter(location %in% loc) %>%
  dplyr::mutate(quantile=quantile %>% as.character() %>% tidyr::replace_na("point"),
                model = "Forecast") %>%
  dplyr::select(-type) %>%
  tidyr::separate(target, into=c("nmo", "target"), sep=" month ahead ") %>%
  dplyr::select(location, date=target_end_date, quantile, value, model) %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(quantile, value) %>%
  dplyr::select(location, date, point, model)

# Bind them
bound <-
  dplyr::bind_rows(real, forecasted) %>%
  dplyr::arrange(date, location) %>%
  dplyr::left_join(dplyr::select(locations, location, location_name), by = "location") %>%
  dplyr::select(-location) %>%
  dplyr::rename(location = location_name)

# Plot
p <-
  bound %>%
  ggplot2::ggplot(ggplot2::aes(date, point)) +
  ggplot2::geom_point(ggplot2::aes(col=model)) +
  ggplot2::geom_line(ggplot2::aes(col=model)) +
  ggplot2::scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  ggplot2::facet_wrap(~location, scales="free", ncol = 1) +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Date", y = NULL) +
  ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
p

# save the monthly forecast
write_csv(submission_list$ensemble, here("inst/extdata/2023-06-01-monthly-TSENS.csv"))
