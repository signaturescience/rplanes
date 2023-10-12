library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinybusy)
library(bslib)
library(ggplot2)
library(dplyr)
library(ragg)
library(rplanes)

options(shiny.useragg = TRUE) # font rendering for auto/custom fonts

#module_sources <- list.files(path = here::here("inst/app/modules/"), full.names = TRUE)
module_sources <- list.files(path = system.file("app/modules/", package = "rplanes"), full.names = TRUE)
sapply(module_sources, source)

# UI Side ####
ui <- tagList(
  # TODO: change includeCSS to system.file("app/style.css", package = "rplanes")
  includeCSS(system.file("app/style.css", package = "rplanes")),
  #includeCSS(here::here("inst/app/style.css")),
  shinybusy::add_busy_spinner(spin = "breeding-rhombus", color = '#073642', position = "full-page", onstart = TRUE),

  shinyjs::useShinyjs(),
  bslib::page_navbar(id = "nav_page",
              title = "Rplanes Explorer",
              theme = bslib::bs_theme(bootswatch = "solar"),
              fillable = FALSE,
              sidebar = bslib::sidebar(
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
                shinyjs::hidden(div(id = "forc_opt",
                                    autonumericInput("horizon", "Forecast Horizon", value = 4, maximumValue = 30, minimumValue = 1, decimalPlaces = 0, align = "center", modifyValueOnWheel = T))),
                materialSwitch("opts", label = "Modify Defaults", value = FALSE, status = "success"),
                shinyjs::hidden(div(id = "add_options",
                                    textInput("width", label = tooltip(trigger = list("Prediction Interval", icon("circle-info")), "Choose prediction interval (95 is default corresponding to a 95% interval) for the forecast data."), value = "95"),

                                    inputsUI("tab2"))),
                actionBttn("run", "Analyze", style = "unite", color = "danger")

              ),
              bslib::nav_panel("Data",
                        dataUI("tab1")),
              bslib::nav_panel("Plots",
                        plotUI("tab2")),
              bslib::nav_panel("Help",
                        htmltools::includeHTML(system.file("app/help_tab.html", package = "rplanes")))
                        #htmltools::includeHTML("help_tab.html"))
                        #htmltools::includeMarkdown("help_tab.md"))
              )


  )

# Server Side ####
server <- function(input, output, session) {
  # Turn on thematic for theme-matched plots
  thematic::thematic_shiny(font = thematic::font_spec(scale = 2))

  observe({
    # unhide the upload custom dataset when choosing "Custom" radiobutton
    shinyjs::toggle(id = "choice_custom", condition = {input$choice %in% "Custom"})
    # unhide additional options upon switch
    shinyjs::toggle(id = "add_options", condition = {input$opts == TRUE})
    shinyjs::toggle(id = "forc_opt", condition = {input$status == TRUE})

  })

  observe({
    updatePickerInput(session = session, inputId = "loc", choices = unique(data_1()$location))
  })

  # when actionBttn is selected automatically go to the plot tab
  observeEvent(input$run, {
    nav_select(id = "nav_page", selected = "Plots")
  })

  # pass in actionBttn to module plots
  btn1 <- reactive({ input$run })

  # pass input$status and input$outcome to module plots
  status <- reactive({ input$status })
  outcome <- reactive({ input$outcome })

  data_1 <- reactive({
    if (input$choice == "Example") {
      # example observed data
      df <- read.csv(system.file("extdata/observed", "hdgov_hosp_weekly.csv", package = "rplanes"))
    } else {
      # Uploading observed data
      req(input$upload_1)
      ext <- tools::file_ext(input$upload_1$name)
      df <- switch(ext,
             csv = read.csv(input$upload_1$datapath),
             validate("Invalid file; Please upload a .csv file"))
    }
    df$date <-  as.Date(df$date)
    df
  })


  data_2 <- reactive({
    if(input$choice == "Example") {
      # example forecast data
      df <- read.csv(system.file("extdata/forecast", "2022-10-31-SigSci-TSENS.csv", package = "rplanes"))
    } else {
      # Uploading forecast data
      req(input$upload_2)
      ext <- tools::file_ext(input$upload_2$name)
      df <- switch(ext,
             csv = read.csv(input$upload_2$datapath),
             validate("Invalid file; Please upload a .csv file"))
    }
    if(input$status){
      df$forecast_date <- as.Date(df$forecast_date)
      df$target_end_date <- as.Date(df$target_end_date)
    } else {
      df$date <- as.Date(df$date)
    }
    df
  })


  prepped_seed <- reactive({
    df = data_1() %>% dplyr::filter(location %in% unique(data_2()$location))
    if(input$status){
      date = unique(df$date)[unique(df$date) < min(data_2()$target_end_date)]
      date = tail(date, 1)
    } else {
      date = unique(df$date)[unique(df$date) < min(data_2()$date)]
      date = tail(date, 1)
    }
    signal <- to_signal(df, outcome = input$outcome, type = "observed", resolution = input$rez)
    prepped_seed  <- plane_seed(signal, cut_date = date)
    prepped_seed
  })

  prepped_forecast <- reactive({
    if (input$choice == "Example"){
      forc <- read_forecast(system.file("extdata/forecast", "2022-10-31-SigSci-TSENS.csv", package = "rplanes"), pi_width = as.numeric(input$width)) %>%
        to_signal(., outcome = "flu.admits", type = "forecast", horizon = as.numeric(input$horizon), resolution = input$rez)
    } else if (input$status){
      forc <- read_forecast(input$upload_2$datapath, pi_width = as.numeric(input$width)) %>%
        filter(location %in% unique(data_1()$location)) %>%
        to_signal(., outcome = input$outcome, type = "forecast", horizon = input$horizon, resolution = input$rez)
    } else {
      df <- read.csv(input$upload_2$datapath)
      df$date <- as.Date(df$date)
      forc <- df %>%
        filter(location %in% unique(data_1()$location)) %>%
        to_signal(., outcome = input$outcome, type = "observed", horizon = input$horizon, resolution = input$rez)
    }
    forc
  })

  dataServer("tab1", data_1 = data_1, data_2 = data_2 )
  plotServer("tab2", data_1 = data_1, seed = prepped_seed, forecast = prepped_forecast, btn1 = btn1, status = status, outcome = outcome)

}

# Run the application
shinyApp(ui = ui, server = server)
