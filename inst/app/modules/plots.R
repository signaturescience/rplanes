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
                        numericInput(ns("tol"), label = tooltip(trigger = list("Choice of Tolerance", icon("circle-info")), "Option to choose the number of allowed repeats before being flagged. Default is 0 (NULL) and repeats will be determined from seed."), value = 0, min = 0, max = 50, step = 1),
                        numericInput(ns("pre"), label = tooltip(trigger = list("Prepend Values", icon("circle-info")), "Option to choose the number of values from seed to evaluate against. Default is 0 (NULL) and value will be determined from seed."),  value = 0, min = 0, max = 365, step = 1)))
  )
}

plotUI <- function(id){
  ns <- NS(id)
  tagList(
    card(full_screen = TRUE, card_header("Scoring Table"), height = "500px", DT::DTOutput(ns("score_table"))),
    shinyjs::hidden(div(id = ns("args3"),
                        layout_column_wrap(width = 1/3, height = 100, fill = FALSE, fillable = FALSE,
                                           pickerInput(ns("loc"), "Choose a Location", choices = "", options =  list(`live-search` = TRUE)),
                                           awesomeRadio(ns("plot_type"), "Choice of Plot", choices = c("Coverage" = "cover", "Difference" = "diff", "Repeat" = "repeats", "Taper" = "taper", "Trend" = "trend"), selected = "cover", inline = TRUE, status = "warning"),
                                           actionButton(ns("plot"), "Plot", class = "btn-success")),
                        layout_column_wrap(width = 1/2,
                                           card(full_screen = TRUE, card_header(textOutput(ns("label"))), height = "500px", plotOutput(ns("plane_plot"))),
                                           card(full_screen = TRUE, card_header(textOutput(ns("label2"))), height = "500px", DT::DTOutput(ns("plane_table")))
                        )))


    )
}

# ~~~~~~~~~~~~~~~~~~~~~~~
# Server Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

plotServer <- function(id, data_1, locations, seed, forecast, btn1, status, outcome) {
  moduleServer(id, function(input, output, session) {

    observe({
      #locations <- generics::intersect(data_1()$location, forecast()$data$location)
      #updatePickerInput(session = session, inputId = "loc", choices = locations)
      updatePickerInput(session = session, inputId = "loc", choices = locations())
    })

    # unhide the locations options and function arguments depending on scoring selection
    observe({
      shinyjs::toggle(id = "args1", condition = {input$score == "all" | input$score == "trend"})
      shinyjs::toggle(id = "args2", condition = {input$score == "all" | input$score == "repeats"})
      shinyjs::toggle(id = "args3", condition = {btn1()})
    })


    scoring <- eventReactive(btn1(), {
      if (input$tol == 0 & input$pre == 0){
        comp_args <- list(trend = list(sig_lvl = input$sig), repeats = list(prepend = NULL, tolerance = NULL))
      } else if (input$tol == 0){
        comp_args <- list(trend = list(sig_lvl = input$sig), repeats = list(prepend = input$pre, tolerance = NULL))
      } else if (input$pre == 0){
        comp_args <- list(trend = list(sig_lvl = input$sig), repeats = list(prepend = NULL, tolerance = input$tol))
      } else {
        comp_args <- list(trend = list(sig_lvl = input$sig), repeats = list(prepend = input$pre, tolerance = input$tol))
      }
      if (status()){
        scores <- plane_score(forecast(), seed(), components = input$score, args = comp_args)
      } else {
        scores <- plane_score(forecast(), seed(), components = c("diff", "repeats"), args = comp_args)
      }
      scores
    })

    output$score_table <- DT::renderDT({
      df <- purrr::map_df(scoring()$scores_summary, as_tibble) %>%
        tidyr::replace_na(list(flagged = "None"))
      df %>% DT::datatable(rownames = F, filter = "top", escape = F, height = "100%", fillContainer = T, extensions =c("Buttons"), options = list(dom = 'lBrtip',
                                                                                                              pageLength = 10,
                                                                                                              lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All")),
                                                                                                              columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                                                              buttons = list('copy', list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download"))
                                                                                                              ))
    })

    # label-header for plotting card
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

    # label-header for table card
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
      cut_date <- seed()[[1]]$meta$cut_date
      dates = unique(as.character(data_1()$date))
      dates = dates[dates <= cut_date]

      values_obs = seed()[[input$loc]]$all_values

      observed_df = data.frame(date = dates,
                               point = values_obs,
                               type = "Observed")

      forecast_df <- forecast()$data
      forecast_df$date = as.character(forecast_df$date)
      if (status()){
        forecast_df = forecast_df %>%
          dplyr::filter(location %in% input$loc) %>%
          dplyr::mutate(type = "Forecast") %>%
          dplyr::select(date, point, lower, upper, type)
      } else {
        forecast_df = forecast_df %>%
          dplyr::filter(location %in% input$loc) %>%
          dplyr::mutate(type = "Observed") %>%
          dplyr::rename(point = outcome()) %>%
          dplyr::select(date, point, type)
      }
      df_plot = bind_rows(observed_df, forecast_df) %>%
        dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d"))
      df_plot
    })

    coverage <- eventReactive(input$plot, {
      item <- paste0(input$loc, "-cover")
      cover <- scoring()$full_results[[item]]
      cover
    })

    difference <- eventReactive(input$plot, {
      item <- paste0(input$loc, "-diff")
      diff <- scoring()$full_results[[item]]
      diff
    })

    repeats <- eventReactive(input$plot, {
      item <- paste0(input$loc, "-repeats")
      df <- scoring()$full_results[[item]]
      df
    })

    taper <- eventReactive(input$plot, {
      item <- paste0(input$loc, "-taper")
      df <- scoring()$full_results[[item]]
      df
    })

    trend <- eventReactive(input$plot, {
      item <- paste0(input$loc, "-trend")
      df <- scoring()$full_results[[item]]$output
      df
    })

    plotting <- eventReactive(input$plot, {

      if(input$plot_type == "cover"){
        date_min <- plot_df() %>% dplyr::filter(type == "Forecast") %>% pull(date) %>% min()
        date_max <- plot_df() %>% dplyr::filter(type == "Forecast") %>% pull(date) %>% max()
        p <- plot_df() %>%
          dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
          ggplot(aes(x = date, y = point)) +
          geom_point(aes(color = type), size = 3) +
          geom_line(alpha = 0.3) +
          labs(title = "Plane Cover", subtitle = paste0("For location: ", input$loc), x = "", y = "Value") +
          annotate("rect", xmin = date_min, xmax = date_max, ymin = coverage()$bounds$lower, ymax = coverage()$bounds$upper, alpha = 0.1, fill = "blue") +
          theme(legend.title=element_blank())
      } else if(input$plot_type == "diff"){
        df_plot2 <- plot_df() %>%
          dplyr::mutate(diff = point - dplyr::lag(point)) %>%
          dplyr::mutate(flag = diff > difference()$maximum_difference)
        p <- df_plot2 %>%
          ggplot(aes(x = date, y = point)) +
          geom_point(aes(color = type), size = 4) +
          geom_point(data = df_plot2 %>% dplyr::filter(flag == TRUE), pch=21, size=7, color="red") +
          labs(title = "Plane Diff", x = "", y = "Value", subtitle = paste0("For location: ", input$loc), caption = paste0("Point greater than ", difference()$maximum_difference, " is circled in red.")) +
          geom_line(alpha = 0.3) +
          theme(legend.title=element_blank())
      } else if(input$plot_type == "repeats"){
        repeat_tbl <- repeats()$repeats
        p <- ggplot() +
          geom_point(data = plot_df(), aes(date, point, color = type), size = 4) +
          geom_point(data = repeat_tbl, aes(date, point, alpha = "Repeat"), shape = 5, size = 6, stroke=2, color = 'darkred') +
          geom_line(data = plot_df(), aes(date, point), alpha = 0.3) +
          labs(title = "Plane Repeat", x = "", y = "Value", subtitle = paste0("For location: ", input$loc)) +
          theme(legend.title=element_blank())
      } else if(input$plot_type == "taper"){
        p <- plot_df() %>%
          ggplot(aes(x = date, y = point)) +
          geom_point(aes(color = type), size = 4) +
          geom_ribbon(aes(date, ymin = lower, ymax = upper), alpha = 0.5) +
          geom_line(alpha = 0.3) +
          labs(title = "Plane Taper", x = "", y = "Value", subtitle = paste0("For location: ", input$loc)) +
          theme(legend.title=element_blank())
      } else {
        p <- trend() %>%
          dplyr::mutate(label = dplyr::case_when(Flagged == TRUE ~ "Flagged Change Point",
                                          TRUE ~ NA)) %>%
          ggplot(aes(x = Date, y = Value)) +
          geom_point(aes(color = Type), size = 4) +
          geom_line(alpha = 0.3) +
          ggrepel::geom_label_repel(aes(label=label, size = 10), color = "black", fill= NA, box.padding = 1, nudge_y = 1.2, segment.linetype = 1, arrow = arrow(length = unit(0.015, "npc"), type = "closed"), na.rm = T, show.legend = F) +
          labs(title = "Plane Trend", x = "", y = "Value", subtitle = paste0("For location: ", input$loc), caption = paste0("Using a significance of ", input$sig)) +
          theme(legend.title=element_blank())
      }
      p
    })

    output$plane_plot <- renderPlot({
        plotting()
    })

    table_df <- eventReactive(input$plot, {
      if (input$plot_type == "cover"){
        df <- data.frame(last_observed_value = coverage()$last_value,
                         lower_bounds = coverage()$bounds$lower,
                         upper_bounds = coverage()$bounds$upper,
                         out_bounds = coverage()$indicator)
      } else if(input$plot_type == "diff"){
        df <- data.frame(Values = difference()$values,
                                Difference = c(NA, difference()$evaluated_differences),
                                Max_difference = difference()$maximum_difference) %>%
          dplyr::mutate(`Diff > Max` = Difference > Max_difference )
      } else if(input$plot_type == "repeats"){
        validate(need(!is.na(repeats()$repeats[1,1]), message = "No Repeats detected."))
        df <- repeats()$repeats %>%
            dplyr::mutate(flagged = "Repeat")
      } else if(input$plot_type == "taper"){
        df <-  plot_df() %>%
          dplyr::filter(type == "Forecast") %>%
          dplyr::mutate(widths = taper()$widths) %>%
          dplyr::mutate(tapering = (widths - dplyr::lag(widths)) < 0) %>%
          tidyr::replace_na(list(tapering = FALSE)) %>%
          dplyr::select(date, lower, upper, widths, tapering) %>%
          dplyr::mutate_if(is.numeric, round, 1)
      } else if(input$plot_type == "trend"){
        df <- trend()
      }
      df

    })

    output$plane_table <- DT::renderDT({
      table_df() %>% DT::datatable(rownames = F, filter = "none", escape = F, extensions =c("Buttons", 'Scroller'), options = list(dom = 'Brtip',
                                                                                                                                   deferRender = TRUE,
                                                                                                                                   scrollY = 200,
                                                                                                                                   scroller = TRUE,
                                                                                                                                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                                                                                                   buttons = list('copy', list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download"))))
    })

  })
}


