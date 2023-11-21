library(shiny)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(rplanes)
library(lubridate)

# list module files and iterate sourcing them to use within the app.
#module_sources <- list.files(path = here::here("inst/app/modules"), full.names = TRUE)
module_sources <- list.files(path = system.file("app/modules/", package = "rplanes"), full.names = TRUE)
sapply(module_sources, source)


# UI SIDE ####

ui <- navbarPage(title = "Rplanes Explorer",
                 inverse = T, # invert color of navigation top bar to black
                 useShinyjs(),  # Set up shinyjs
                 tabPanel(title = "Plots",
                          fluidPage(
                            sidebarLayout(position = "left",
                                          sidebarPanel(width = 3,
                                                       prettyRadioButtons("choice", "Choose Dataset", choices = c("Custom", "Example"), selected = "Custom",  status = "warning", inline = TRUE, icon = icon("check"), bigger = TRUE),
                                                       shinyjs::hidden(div(id = "choice_custom",
                                                                           fileInput("upload_1", label = "Upload Observed Data", multiple = F, accept = ".csv"),
                                                                           fileInput("upload_2", label = "Upload Comparison", multiple = F, accept = ".csv")
                                                       )),
                                                       awesomeRadio("status", "Type of signal to be evaluated", choices = c("Forecast", "Observed"), selected= "Forecast", inline = T, status = "warning"),
                                                       awesomeRadio("rez", "Resolution", choices = "", inline = T, status = "info"),
                                                       textInput("outcome", label = "Outcome", value = "flu.admits"),
                                                       shinyjs::hidden(div(id = "forc_opt",
                                                                           shinyWidgets::autonumericInput("horizon", "Forecast Horizon", value = 4, maximumValue = 30, minimumValue = 1, decimalPlaces = 0, align = "center", modifyValueOnWheel = T))),
                                                       materialSwitch("opts", label = "Modify Defaults", value = FALSE, status = "success"),
                                                       shinyjs::hidden(div(id = "add_options",
                                                                           textInput("width", label = "Prediction Interval", value = "95"),
                                                                           pickerInput(inputId = "score", label = "PLANES Component(s)", choices = "", options = list(`actions-box` = TRUE), multiple = T),
                                                                           inputsUI("tab2")
                                                       )),
                                                       actionBttn("run", "Analyze", style = "unite", color = "danger"),
                                                       actionBttn("reset", "Reset", style = "stretch", color = "warning")
                                          ),
                                          mainPanel(
                                            tabsetPanel(id = "tabs1",
                                                        tabPanel("Scoring Table",
                                                                 tableUI("tab2")),
                                                        tabPanel("Data Tables",
                                                                 dataUI("tab1"))),
                                            plotUI("tab2")

                                          )
                                          ), # sidebarLayout


                          )), # plots tab
                 tabPanel(title = "Help",
                          htmltools::includeHTML(system.file("app/help_tab.html", package = "rplanes")))
                          #htmltools::includeHTML(here::here("inst/app/help_tab.html"))),
) # UI end


# SERVER SIDE ####

server <- function(input, output, session){

  observe({
    # unhide the upload custom dataset when choosing "Custom" radiobutton
    shinyjs::toggle(id = "choice_custom", condition = {input$choice %in% "Custom"})
    # unhide additional options upon switch
    shinyjs::toggle(id = "add_options", condition = {input$opts == TRUE})
    shinyjs::toggle(id = "forc_opt", condition = {input$status == "Forecast"})
    })

  # update scoring options based on user input of observed or forecast comparison
  observe({
    if(input$status == "Observed"){
      score_opt = c("Difference" = "diff", "Repeat" = "repeat")
      updatePickerInput(session = session, inputId = "score", choices = score_opt, selected = c("diff", "repeat"))
    } else {
      score_opt = c("Coverage" = "cover", "Difference" = "diff", "Repeat" = "repeat", "Taper" = "taper", "Trend" = "trend")
      updatePickerInput(session = session, inputId = "score", choices = score_opt, selected = c("Coverage" = "cover", "Difference" = "diff", "Repeat" = "repeat", "Taper" = "taper", "Trend" = "trend"))
    }
  })

    # Update the resolution choice depending on the type of data in the observed dataset (data_1)
    # This ensures no error when data is weekly the resolution weekly will be selected.
    # TODO: Perhaps we can do away with this selection, making this as part of the backend instead. Leaving this for now.

    observe({
        if(input$choice %in% "Example"){
            duration = lubridate::interval(start = ymd(data_1()$date[1]), end = ymd(data_1()$date[2])) %>% as.period(unit = "day")
            x = duration@day
            if(x == 7){
                y = list("Weekly" = "weeks")
            } else if(x < 7){
                y = list("Daily" = "days")
            } else {
                y = list("Monthly" = "months")
            }
        } else if (input$choice %in% "Custom"){
            req(input$upload_1)
            duration = lubridate::interval(start = ymd(data_1()$date[1]), end = ymd(data_1()$date[2])) %>% as.period(unit = "day")
            x = duration@day
            if(x == 7){
                y = list("Weekly" = "weeks")
            } else if(x < 7){
                y = list("Daily" = "days")
            } else {
                y = list("Monthly" = "months")
            }
        }
        updateAwesomeRadio(session = session, inputId = "rez", choices = y)
    })

    # pass in actionBttn to module plots
    btn1 <- reactive({ input$run })
    btn2 <- reactive({ input$reset })

    # pass input$status, input$outcome, input$score to module plots
    status <- reactive({ input$status })
    outcome <- reactive({ input$outcome })
    score <- reactive({ input$score })

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

    # function to check if date can be converted to the format yyyy-mm-dd
    is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), format = '%Y-%m-%d'))


    data_2 <- reactive({
        if(input$choice == "Example") {
            # example forecast data
            df <- read.csv(system.file("extdata/forecast", "2022-10-31-SigSci-TSENS.csv", package = "rplanes"))
        } else {
            # Uploading forecast data
            req(input$upload_2)
            ext <- tools::file_ext(input$upload_2$datapath)
            df <- switch(ext,
                         csv = read.csv(input$upload_2$datapath),
                         validate("Invalid file; Please upload a .csv file"))
        }
        if(input$status == "Forecast"){
            df$forecast_date <- as.Date(df$forecast_date, format = "%Y-%m-%d")
            df$target_end_date <- as.Date(df$target_end_date, format = "%Y-%m-%d")
            width <- round(rplanes:::q_boundary(as.numeric(input$width)), 2)
            quant_list <- round(c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), 2)
            validate(need(all(width %in% quant_list), message = "Quantiles unavailable for width specified."))
            df <- df %>%
                dplyr::mutate(quantile = ifelse(is.na(df$quantile), 0.5, df$quantile)) %>%
                filter(quantile %in% width)

        } else {
            validate(need(is.convertible.to.date(df$date[1]), message = "Columns containing dates need to be formatted like: 2022-10-31"))
            df$date <- as.Date(df$date, format = "%Y-%m-%d")
        }
        df
    }) %>% bindEvent(input$width, input$choice, input$upload_2)


    prepped_seed <- reactive({
        df = data_1() %>% dplyr::filter(location %in% unique(data_2()$location))
        if(input$status == "Forecast"){
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
        } else if (input$status == "Forecast"){
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

    # get all intersecting locations between the datasets to use as input$loc in plots module
    locations <- reactive({
        generics::intersect(data_1()$location, data_2()$location)
    })

    dataServer("tab1", data_1 = data_1, data_2 = data_2 )

    plotServer("tab2", score = score, data_1 = data_1, locations = locations, seed = prepped_seed, forecast = prepped_forecast, btn1 = btn1, status = status, outcome = outcome, btn2 = btn2)

    # reset all inputs including the ones in the modules
    observeEvent(input$reset,{
      session$reload()
    })

} # server end



# Run the application
shinyApp(ui = ui, server = server)

