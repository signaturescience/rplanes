# ~~~~~~~~~~~~~~~~~~~~~~~
# UI Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

inputsUI <- function(id){
  ns <- NS(id)
  tagList(
    radioGroupButtons(ns("score"), "Choice of scoring", choices = c("All", "Coverage", "Difference", "Repeat", "Taper"), individual = F, direction = "vertical", justified = F, width = "100%",
                      checkIcon = list(
                        yes = icon("square-check"),
                        no = icon("square"))),
    shinyjs::hidden(div(id = ns("opt"),
                        pickerInput(ns("loc"), "Choose a Location", choices = "", options =  list(`live-search` = TRUE)))),
    actionBttn(ns("run"), "Analyze", style = "unite", color = "danger")
  )
}

plotUI <- function(id){
  ns <- NS(id)
  tagList(
    layout_column_wrap(width = 1/2,
      card(full_screen = TRUE, card_header("Coverage"), height = "400px", plotOutput(ns("plot_cover"))),
      card(full_screen = TRUE, card_header("Coverage Table"), height = "400px", DT::DTOutput(ns("table_cover"))),
      card(full_screen = TRUE, card_header("Difference"), height = "400px"),
      card(full_screen = TRUE, card_header("Difference Table"), height = "400px"),
      card(full_screen = TRUE, card_header("Repeat"), height = "400px"),
      card(full_screen = TRUE, card_header("Repeat Table"), height = "400px"),
    )

  )

}

# ~~~~~~~~~~~~~~~~~~~~~~~
# Server Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

plotServer <- function(id, data_1, data_2, seed, forecast) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePickerInput(session = session, inputId = "loc", choices = unique(data_1()$location))
    })

    # unhide the locations options if they choose individual plane_functions
    observe({
      shinyjs::toggle(id = "opt", condition = {input$score != "All"})
    })

    plot_df <- eventReactive(input$run, {
      cut_date <- seed()[[1]]$meta$cut_date
      dates = unique(as.character(data_1()$date))
      dates = dates[dates <= cut_date]

      values_obs = seed()[[input$loc]]$all_values

      observed_df = data.frame(date = dates,
                               point = values_obs,
                               type = "Observed")

      forecast_df <- forecast()$data
      forecast_df$date = as.character(forecast_df$date)
      forecast_df = forecast_df %>%
        filter(location %in% input$loc) %>%
        mutate(type = "Forecast") %>%
        select(date, point, type)

      df_plot = bind_rows(observed_df, forecast_df)
      df_plot
    })

    coverage <- eventReactive(input$run, {
      validate(need(input$score == "Coverage", message = "Choose the Coverage option"))
      cover <- plane_cover(location = input$loc, input = forecast(), seed = seed())
      cover
    })

    output$plot_cover <- renderPlot({
      date_min <- plot_df() %>% filter(type == "Forecast") %>% pull(date) %>% min()
      date_max <- plot_df() %>% filter(type == "Forecast") %>% pull(date) %>% max()
      p <- plot_df() %>%
        ggplot(aes(x = date, y = point, color = type)) +
        geom_point() +
        labs(title = "Plane Cover", subtitle = paste0("For location: ", input$loc), x = "", y = "Value") +
        theme(axis.text.x = element_text(angle = 90)) +
        annotate("rect", xmin = date_min, xmax = date_max, ymin = coverage()$bounds$lower, ymax = coverage()$bounds$upper, alpha = 0.1, fill = "blue")
      p
    })

    output$table_cover <- DT::renderDT({
      df <- data.frame(last_observed_value = coverage()$last_value,
                       lower_bounds = coverage()$bounds$lower,
                       upper_bounds = coverage()$bounds$upper,
                       out_bounds = coverage()$indicator)
      df %>% DT::datatable(rownames = F, filter = "none", escape = F, extensions =c("Buttons"), options = list(dom = 'Brtip',
                                                                                                               columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                                                               buttons = list('copy', list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download"))))

    })






  })
}
