library(shiny)
library(shinyWidgets)
library(bslib)
library(waiter)
library(ggplot2)
library(plotly)
library(dplyr)
library(ragg)

options(shiny.useragg = TRUE) # font rendering for auto/custom fonts


# UI Side ####
ui <- page_sidebar(title = "Rplanes Explorer",
  useWaiter(),
  waiterOnBusy(html = spin_terminal(), color = transparent(0)),
  theme = bs_theme(bootswatch = "solar"),
  sidebar = sidebar(
    width = 300,
    bg = '#6c757d',
    position = "left",
    fileInput("upload_1", "Upload Observed Data", multiple = F, accept = c(".csv", ".tsv")),
    fileInput("upload_2", "Upload Forecast", multiple = F, accept = c(".csv", ".tsv"))


    ),

  #sidebarLayout(
  #  sidebarPanel(width = 4,
  #    fileInput("upload_1", "Upload Observed Data", multiple = F, accept = c(".csv", ".tsv")),
  #    fileInput("upload_2", "Upload Forecast", multiple = F, accept = c(".csv", ".tsv")),

    mainPanel(
     tabsetPanel(type = 'pills',
                  tabPanel("Plots",
                           p("some text"),
                           plotOutput("plot")),
                  tabPanel("Another one",
                           h3("something"))))



)

# Server Side ####
server <- function(input, output, session) {
  waiter_hide()
  # Turn on thematic for theme-matched plots
  thematic::thematic_shiny(font = "auto")

  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() +
      geom_smooth()
  }, res = 96)

  data_1 <- reactive({
    req(input$upload_1)

    ext <- tools::file_ext(input$upload_1$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  data_2 <- reactive({
    req(input$upload_2)

    ext <- tools::file_ext(input$upload_2$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
