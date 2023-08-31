library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(waiter)
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
  useWaiter(),
  useShinyjs(),
  waiterOnBusy(html = spin_terminal(), color = transparent(0)),
  page_navbar(title = "Rplanes Explorer",
              theme = bs_theme(bootswatch = "solar"),
              sidebar = sidebar(
                width = 300,
                bg = '#6c757d',
                position = "left",
                prettyRadioButtons("choice", "Choose Dataset", choices = c("Example", "Custom"), status = "warning", inline = TRUE, icon = icon("check"), bigger = TRUE),
                shinyjs::hidden(div(id = "choice_custom",
                                    fileInput("upload_1", label = tooltip(trigger = list("Upload Observed Data", bsicons::bs_icon("info-circle")), "Upload only a .csv file"), multiple = F, accept = ".csv"),
                                    fileInput("upload_2", label = tooltip(trigger = list("Upload Forecast", bsicons::bs_icon("info-circle")), "Upload only a .csv file"), multiple = F, accept = ".csv"))),
                tableUI("tab2")
              ),
              nav_panel("Data",
                        dataUI("tab1")),
              nav_panel("Table"),
              nav_panel("plot",
                        plotOutput("plot"))
              )


  )

# Server Side ####
server <- function(input, output, session) {
  waiter_hide()
  # Turn on thematic for theme-matched plots
  thematic::thematic_shiny(font = "auto")

  # unhide the upload custom dataset when choosing "Custom" radiobutton
  observe({
    shinyjs::toggle(id = "choice_custom", condition = {input$choice %in% "Custom"})
  })

  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() +
      geom_smooth()
  }, res = 96)

  data_1 <- reactive({
    if (input$choice == "Example") {
      # example observed data
      df <- read.csv(system.file("extdata/observed", "hdgov_hosp_weekly.csv", package = "rplanes"))
      df$date = as.Date(df$date)
      df
    } else {
      # Uploading observed data
      req(input$upload_1)
      ext <- tools::file_ext(input$upload_1$name)
      switch(ext,
             csv = vroom::vroom(input$upload_1$datapath, delim = ","),
             validate("Invalid file; Please upload a .csv file"))
      csv
    }
  })


  data_2 <- reactive({
    if(input$choice == "Example") {
      # example forecast data
      df <- read.csv(system.file("extdata/forecast", "2023-02-06-SigSci-TSENS.csv", package = "rplanes"))
      df
    } else {
      # Uploading forecast data
      req(input$upload_2)
      ext <- tools::file_ext(input$upload_2$name)
      switch(ext,
             csv = vroom::vroom(input$upload_2$datapath, delim = ","),
             validate("Invalid file; Please upload a .csv file"))
      csv
    }
  })

  dataServer("tab1", data_1 = data_1, data_2 = data_2)
  tableServer("tab2", data_1 = data_1, data_2 = data_2)

}

# Run the application
shinyApp(ui = ui, server = server)
