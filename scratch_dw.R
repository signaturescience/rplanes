library(tidyverse)
library(rplanes)


# repeat value check for plausibility score #4 ####

# Take the to_signal forecast or observed to check if the values are repeated too many times
plane_repeat <- function(input, k = 3){


  # check the class of the input, must be forecast or observed as created by to_signal
  if(is_observed(input)) {
    tmp_dat <- input$data %>%
      group_by(location) %>%
      arrange(date) %>%
      mutate(repeated = ifelse(
        slider::slide_vec(.data[[input$outcome]], var, .before = k) == 0, TRUE, FALSE)
        )
  } else if(is_forecast(input)) {
    tmp_dat <- input$data %>%
      group_by(location) %>%
      arrange(date) %>%
      mutate(repeated = ifelse(
        slider::slide_vec(.data$point, var, .before = input$horizon-1) == 0, TRUE, FALSE) # if the variation = 0 for consecutive horizon's points return TRUE else FALSE
      )
  }
  if(any(tmp_dat$repeated %in% TRUE)){
    ind <- any(tmp_dat$repeated %in% TRUE, na.rm = T)
    loc <- tmp_dat$location[tmp_dat$repeated %in% TRUE]
    dates <- tmp_dat$date[tmp_dat$repeated %in% TRUE]
    horizon <- tmp_dat$horizon[tmp_dat$repeated %in% TRUE]

    return(list(indicator = ind, locations = loc, dates = dates, horizon = horizon))

  } else {
    ind <- any(tmp_dat$repeated %in% TRUE, na.rm = T)
    return(list(indicator = ind))
  }
}

