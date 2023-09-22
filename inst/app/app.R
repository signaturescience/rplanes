library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinybusy)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(ragg)
library(rplanes)

options(shiny.useragg = TRUE) # font rendering for auto/custom fonts

## load global functions and iterate through module files to load in module functions
#source(system.file("app/global_functions.R", package = "rplanes"))
module_sources <- list.files(path = here::here("inst/app/modules/"), full.names = TRUE)
#module_sources <- list.files(path = system.file("app/modules", package = "rplanes"), full.names = TRUE)
sapply(module_sources, source)

# UI Side ####
ui <- tagList(
  # TODO: change includeCSS to system.file("app/stype.css", package = "rplanes")
  includeCSS(here::here("inst/app/style.css")),
  add_busy_spinner(spin = "breeding-rhombus", color = '#073642', position = "full-page", onstart = TRUE),

  useShinyjs(),
  page_navbar(title = "Rplanes Explorer",
              theme = bs_theme(bootswatch = "solar"),
              fillable = FALSE,
              sidebar = sidebar(
                width = 300,
                bg = '#6c757d',
                position = "left",
                prettyRadioButtons("choice", "Choose Dataset", choices = c("Example", "Custom"), status = "warning", inline = TRUE, icon = icon("check"), bigger = TRUE),

                shinyjs::hidden(div(id = "choice_custom",
                                    fileInput("upload_1", label = tooltip(trigger = list("Upload Observed Data", icon("circle-info")), "Upload only a .csv file"), multiple = F, accept = ".csv"),
                                    fileInput("upload_2", label = tooltip(trigger = list("Upload Comparison", icon("circle-info")), "Upload data to compare to observed data in .csv format"), multiple = F, accept = ".csv"),
                                    h6("Is the Comparison a Forecast"),
                                    prettyToggle("status", label_on = "Yes", icon_on = icon("check"), status_on = "success", label_off = "No", icon_off = icon("remove"), status_off = "warning", value = TRUE))),
                awesomeRadio("rez", "Resolution", choices = c("Daily" = "days", "Weekly" = "weeks", "Monthly" = "months"), selected = "Weekly", inline = T, status = "info"),
                textInput("outcome", label = tooltip(trigger = list("Type of Outcome", icon("circle-info")), "Type the name of the observed outcome column."), value = "flu.admits"),
                pickerInput("horizon", "Forecast Horizon", choices = c(1,2,3,4), selected = 4),
                textInput("width", label = tooltip(trigger = list("Prediction Interval", icon("circle-info")), "Choose prediction interval (95 is default corresponding to a 95% interval) for the forecast data."), value = "95"),

                inputsUI("tab2")

              ),
              nav_panel("Data",
                        dataUI("tab1")),
              nav_panel("Plots",
                        #plotOutput("plot"),
                        plotUI("tab2")),
              nav_panel("Help",
                        htmltools::includeMarkdown("help_tab.md"))
              )


  )

# Server Side ####
server <- function(input, output, session) {
  # Turn on thematic for theme-matched plots
  thematic::thematic_shiny(font = thematic::font_spec(scale = 2))

  # unhide the upload custom dataset when choosing "Custom" radiobutton
  observe({
    shinyjs::toggle(id = "choice_custom", condition = {input$choice %in% "Custom"})
    # Dates must be before forecasting target end dates
    # must make the date a character to pass into choices it was outputting the date as a numeric
    #dates <- unique(data_1()$date)[unique(data_1()$date) < min(unique(data_2()$target_end_date))]
    #updatePickerInput(session = session, inputId = "date", choices = unique(as.character(dates)))
  })
  observe({
    updatePickerInput(session = session, inputId = "loc", choices = unique(data_1()$location))
  })

  data_1 <- reactive({
    if (input$choice == "Example") {
      # example observed data
      df <- read.csv(system.file("extdata/observed", "hdgov_hosp_weekly.csv", package = "rplanes"))
    } else {
      # Uploading observed data
      req(input$upload_1)
      ext <- tools::file_ext(input$upload_1$name)
      switch(ext,
             df= read.csv(input$upload_1$datapath),
             validate("Invalid file; Please upload a .csv file"))
    }
    df$date <-  as.Date(df$date)
    df
  })


  data_2 <- reactive({
    if(input$choice == "Example") {
      # example forecast data
      df <- read.csv(system.file("extdata/forecast", "2023-02-06-SigSci-TSENS.csv", package = "rplanes"))
    } else {
      # Uploading forecast data
      req(input$upload_2)
      ext <- tools::file_ext(input$upload_2$name)
      switch(ext,
             df = read.csv(input$upload_2$datapath),
             validate("Invalid file; Please upload a .csv file"))
    }
    df$forecast_date <- as.Date(df$forecast_date)
    df$target_end_date <- as.Date(df$target_end_date)
    df
  })


  prepped_seed <- reactive({
    signal <- to_signal(data_1(), outcome = input$outcome, type = "observed", resolution = input$rez)
    date = unique(data_1()$date)[unique(data_1()$date) < min(data_2()$target_end_date)]
    date = tail(date, 1)
    prepped_seed  <- plane_seed(signal, cut_date = date)
    prepped_seed
  })

  prepped_forecast <- reactive({
    if (input$choice == "Example"){
      forc <- read_forecast(system.file("extdata/forecast", "2023-02-06-SigSci-TSENS.csv", package = "rplanes"), pi_width = as.numeric(input$width)) %>%
        to_signal(., outcome = "flu.admits", type = "forecast", horizon = as.numeric(input$horizon), resolution = input$rez)
    } else if (input$status){
      forc <- read_forecast(input$upload_2$datapath, pi_width = as.numeric(input$width)) %>%
        to_signal(., outcome = input$outcome, type = "forecast", horizon = input$horizon, resolution = input$rez)
    } else {
      forc <- read_forecast(input$upload_2$datapath, pi_width = as.numeric(input$width)) %>%
        to_signal(., outcome = input$outcome, type = "observed", horizon = input$horizon, resolution = input$rez)
    }
    forc
  })

  dataServer("tab1", data_1 = data_1, data_2 = data_2 )
  plotServer("tab2", data_1 = data_1, data_2 = data_2, seed = prepped_seed, forecast = prepped_forecast)

}

# Run the application
shinyApp(ui = ui, server = server)
