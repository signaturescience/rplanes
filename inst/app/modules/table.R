# ~~~~~~~~~~~~~~~~~~~~~~~
# UI Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

tableUI <- function(id){
  ns <- NS(id)
  tagList(
    awesomeRadio(ns("rez"), "Resolution", choices = c("Daily" = "days", "Weekly" = "weeks", "Monthly" = "months"), selected = "Weekly", inline = T, status = "info"),
    pickerInput(ns("horizon"), "Forecast Horizon", choices = c(1,2,3,4), selected = 4),
    pickerInput(ns("date"), label = tooltip(trigger = list("Seed Date", bsicons::bs_icon("info-circle")), "Choose a cut-off date for observed data."), choices = "", options = list(`live-search` = TRUE)),
    textInput(ns("width"), label = tooltip(trigger = list("Prediction Interval", bsicons::bs_icon("info-circle")), "Choose prediction interval (95 is default corresponding to a 95% interval) for the forecast data."), value = "95"),
    radioGroupButtons(ns("score"), "Choice of scoring", choices = c("All", "Coverage", "Difference", "Repeat", "Taper"), individual = F, direction = "vertical", justified = F, width = '200px',
                      checkIcon = list(
                        yes = icon("square-check"),
                        no = icon("square")))

  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~
# Server Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

tableServer <- function(id, data_1, data_2) {
  moduleServer(id, function(input, output, session) {

    observe({
      # must make the date a character to pass into choices it was outputting the date as a numeric
      updatePickerInput(session = session, inputId = "date", choices = unique(as.character(data_1()$date)))
    })

    prepped_seed <- reactive({
      signal <- to_signal(data_1(), outcome = "flu.admits", type = "observed", resolution = input$rez)
      plane_seed(signal, cut_date = input$date)
    })

    prepped_forecast <- reactive({
      if (input$choice == "Example"){
        read_forecast(system.file("extdata/forecast", "2023-02-06-SigSci-TSENS.csv", package = "rplanes"), pi_width = as.numeric(input$width)) %>%
          to_signal(., outcome = "flu.admits", type = "forecast", horizon = input$horizon, resolution = input$rez)
      } else {
        read_forecast(input$upload_2$datapath, pi_width = as.numeric(input$width)) %>%
          to_signal(., outcome = "flu.admits", type = "forecast", horizon = input$horizon, resolution = input$rez)
      }

    })


  })
}
