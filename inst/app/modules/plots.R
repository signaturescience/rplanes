# ~~~~~~~~~~~~~~~~~~~~~~~
# UI Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

inputsUI <- function(id){
  ns <- NS(id)
  tagList(
    radioGroupButtons(ns("score"), "Choice of scoring", choices = c("All"= "all", "Coverage" = "cover", "Difference" = "diff", "Repeat" = "repeats", "Taper" = "taper", "Trend" = "trend"), individual = F, direction = "vertical", justified = F, width = "100%",
                      checkIcon = list(
                        yes = icon("square-check"),
                        no = icon("square"))),
    shinyjs::hidden(div(id = ns("args1"),
                        sliderTextInput(ns("sig"), "Choice of Significance for Trend", choices = seq(from = 0.01, to = 0.2, by = 0.01), selected = 0.1, grid = TRUE))),
    shinyjs::hidden(div(id = ns("args2"),
                        numericInput(ns("tol"), label = tooltip(trigger = list("Choice of Tolerance", icon("circle-info")), "Option to choose the number of allowed repeats before being flagged. Default is NULL and repeats will be determined from seed."), value = 2, min = 2, max = 50, step = 1),
                        numericInput(ns("pre"), label = tooltip(trigger = list("Prepend Values", icon("circle-info")), "Option to choose the number of values from seed to evaluate against. Default is NULL and value will be determined from seed."),  value = 2, min = 1, max = 365, step = 1))),
    actionBttn(ns("run"), "Analyze", style = "unite", color = "danger")
  )
}

plotUI <- function(id){
  ns <- NS(id)
  tagList(
    card(full_screen = TRUE, card_header("Scoring Table"), DT::DTOutput(ns("score_table"))),
    layout_column_wrap(width = "100px", height = 100, fill = FALSE, fillable = FALSE,
                       pickerInput(ns("loc"), "Choose a Location", choices = "", options =  list(`live-search` = TRUE)),
                       awesomeRadio(ns("plot_type"), "Choice of Plot", choices = c("Coverage" = "cover", "Difference" = "diff", "Repeat" = "repeats", "Taper" = "taper", "Trend" = "trend"), selected = "cover", inline = TRUE, status = "warning"),
                       actionButton(ns("plot"), "Plot", class = "btn-success")),
    layout_column_wrap(width = 1/2,
                       card(full_screen = TRUE, card_header(textOutput(ns("label"))), height = "400px", plotOutput(ns("plot_cover"))),
                       card(full_screen = TRUE, card_header(textOutput(ns("label2"))), height = "400px", DT::DTOutput(ns("table_cover")))
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

    # unhide the locations options and function arguments depending on scoring selection
    observe({
      shinyjs::toggle(id = "args1", condition = {input$score == "all" | input$score == "trend"})
      shinyjs::toggle(id = "args2", condition = {input$score == "all" | input$score == "repeats"})
    })

    scoring <- eventReactive(input$run, {
      comp_args <- list(trend = list(sig_lvl = input$sig), repeats = list(prepend = input$pre, tolerance = input$tol))
      scores <- plane_score(forecast(), seed(), components = input$score, args = comp_args)
      scores
    })

    output$score_table <- DT::renderDT({
      scoring()$scores_raw %>% DT::datatable(rownames = F, filter = "none", escape = F, extensions =c("Buttons", "Scroller"), options = list(dom = 'Brtip',
                                                                                                                                             deferRender = TRUE,
                                                                                                                                             scrollY = 200,
                                                                                                                                             scroller = TRUE,
                                                                                                                                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                                                                                             buttons = list('copy', list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download"))))
    })

    output$label <- renderText({
      if(input$plot_type == "cover"){
        "Coverage Plot"
      } else if(input$plot_type == "diff") {
        "Difference Plot"
      } else if(input$plot_type == "repeats"){
        "Repeat Plot"
      } else if(input$plot_type == "taper"){
        "Taper Plot"
      } else {
        "Trend Plot"
      }
    })

    output$label2 <- renderText({
      if(input$plot_type == "cover"){
        "Coverage Table"
      } else if(input$plot_type == "diff") {
        "Difference Table"
      } else if(input$plot_type == "repeats"){
        "Repeat Table"
      } else if(input$plot_type == "taper"){
        "Taper Table"
      } else {
        "Trend Table"
      }
    })

    plot_df <- eventReactive(input$plot, {
      validate(need(!is.null(scoring()), message = "Scoring needs to be run. See the above table."))
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

    coverage <- eventReactive(input$plot, {
      item <- paste0(input$loc, "-cover")
      cover <- scoring()$full_results[[item]]
      cover
    })



    output$plot_cover <- renderPlot({
      date_min <- plot_df() %>% filter(type == "Forecast") %>% pull(date) %>% min()
      date_max <- plot_df() %>% filter(type == "Forecast") %>% pull(date) %>% max()
      if(input$plot_type == "cover"){
        p <- plot_df() %>%
          ggplot(aes(x = date, y = point, color = type)) +
          geom_point() +
          labs(title = "Plane Cover", subtitle = paste0("For location: ", input$loc), x = "", y = "Value") +
          theme(axis.text.x = element_text(angle = 90)) +
          annotate("rect", xmin = date_min, xmax = date_max, ymin = coverage()$bounds$lower, ymax = coverage()$bounds$upper, alpha = 0.1, fill = "blue")
        p
      } else if(input$plot_type == "diff"){
        df_plot2 = plot_df() %>%
          mutate(diff = point - lag(point),
                 mid = zoo::rollapply(date, 2, mean, by = 1, align = "left", partial = T),
                 pos = zoo::rollapply(point, 2, mean, by = 1, align = "left", partial = T))

        text_df = data.frame(text_x = df_plot2$mid,
                             text_y = df_plot2$pos,
                             text = c(df_plot2$diff[-1], NA))
        p <- df_plot2 %>%
          ggplot(aes(x = date, y = point, color = type)) +
          geom_point() +
          geom_text(aes(x = text_x, y = text_y, label = text), data = text_df, size = 3, color = "black") +
          geom_line(linetype = "dotted")
        p
      }

    })

    table_df <- eventReactive(input$plot, {
      if (input$plot_type == "cover"){
        df <- data.frame(last_observed_value = coverage()$last_value,
                         lower_bounds = coverage()$bounds$lower,
                         upper_bounds = coverage()$bounds$upper,
                         out_bounds = coverage()$indicator)
        df
      }

    })

    output$table_cover <- DT::renderDT({
      table_df() %>% DT::datatable(rownames = F, filter = "none", escape = F, extensions =c("Buttons"), options = list(dom = 'Brtip',
                                                                                                               columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                                                               buttons = list('copy', list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download"))))

    })






  })
}
