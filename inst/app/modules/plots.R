# ~~~~~~~~~~~~~~~~~~~~~~~
# UI Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

inputsUI <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(div(id = ns("args1"),
                        numericInput(ns("sig"), "Choice of Significance for Trend", value = 0.1, min = 0, max = 1, step = 0.01))),
    shinyjs::hidden(div(id = ns("args2"),
                        numericInput(ns("tol"), label = "Choice of Tolerance", value = 0, min = 0, max = 50, step = 1),
                        numericInput(ns("pre"), label = "Prepend Values",  value = 0, min = 0, max = 365, step = 1)))
  )
}

plotUI <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(div(id = ns("args3"),
                        fluidRow(
                          tags$h3("Overall Scoring"),
                          column(width = 4,
                                 plotOutput(ns("score_plot")) %>% shinycssloaders::withSpinner(type = 6, size=2, color = "#246479")),
                          column(width = 8,
                                 DT::DTOutput(ns("score_table")))
                        ),
                        fluidRow(
                          tags$h3("Individual Scoring"),
                          column(width = 3,
                                 pickerInput(ns("loc"), "Location", choices = "", options =  list(`live-search` = TRUE))),
                          column(width = 9,
                                 awesomeRadio(ns("plot_type"), "Component", choices = "", inline = TRUE, status = "warning")
                          ),
                          plotOutput(ns("plane_plot"))
                        ))))

}

# ~~~~~~~~~~~~~~~~~~~~~~~
# Server Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

plotServer <- function(id, score, data_1, locations, seed, forecast, btn1, status, outcome, btn2) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePickerInput(session = session, inputId = "loc", choices = locations())
      choice <- c("Coverage" = "cover", "Difference" = "diff", "Repeat" = "repeat", "Taper" = "taper", "Trend" = "trend")
      plot_choice <- choice[choice %in% score()]
      updateAwesomeRadio(session = session, inputId = "plot_type", choices = plot_choice, inline = TRUE, status = "warning")
    })



    # unhide the locations options and function arguments depending on scoring selection
    observe({
      shinyjs::toggle(id = "args1", condition = {"trend" %in% score()})
      shinyjs::toggle(id = "args2", condition = {"repeat" %in% score()})
      shinyjs::toggle(id = "args3", condition = {btn1()})
    })

    # run the scoring using logic to modify the args parameter in plane_score for the repeats function
    # This applies to the repeats option, was not taking my direct inputs unless I specified it out into a list like below.
    scoring <- eventReactive(btn1(),{

      if (input$tol == 0 & input$pre == 0){
        comp_args <- list(trend = list(sig_lvl = input$sig), `repeat` = list(prepend = NULL, tolerance = NULL))
      } else if (input$tol == 0){
        comp_args <- list(trend = list(sig_lvl = input$sig), `repeat` = list(prepend = input$pre, tolerance = NULL))
      } else if (input$pre == 0){
        comp_args <- list(trend = list(sig_lvl = input$sig), `repeat` = list(prepend = NULL, tolerance = input$tol))
      } else {
        comp_args <- list(trend = list(sig_lvl = input$sig), `repeat` = list(prepend = input$pre, tolerance = input$tol))
      }
      scores <- plane_score(forecast(), seed(), components = score(), args = comp_args)
      scores
    })

    output$score_table <- DT::renderDataTable(server = FALSE, {
      df <- purrr::map_df(scoring()$scores_summary, as_tibble) %>%
        tidyr::replace_na(list(flagged = "None"))
      DT::datatable(
        df,
        extensions = "Buttons",
        filter = "top",
        selection = "none", #this is to avoid select rows if you click on the rows
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          autoWidth = FALSE,
          dom = 'Blrtip',
          buttons = list(
            'copy',
            # this will download the entire dataset using modifier = list(page = "all")
            list(
              extend = 'collection',
              buttons = list(
                list(extend = "csv", filename = "score_data", exportOptions = list(
                  columns = ":visible",modifier = list(page = "all"))
                ),
                list(extend = 'excel', filename = "score_data", title = NULL,
                     exportOptions = list(columns = ":visible", modifier = list(page = "all"))),
                list(extend = 'pdf', filename = "score_data", exportOptions = list(
                  columns = ":visible", modifier = list(page = "all")))),
              text = 'Download')

          ),
          # add the option to display more rows as a length menu
          lengthMenu = list(c(10, 30, 50, -1),
                            c('10', '30', '50', 'All'))
        ),
        class = "display"
      )
      })

    plot_df <- reactive({
      cut_date <- seed()[[1]]$meta$cut_date
      dates <- unique(as.character(data_1()$date))
      dates <- dates[dates <= cut_date]

      values_obs <- seed()[[input$loc]]$all_values

      observed_df <- data.frame(date = dates,
                               point = values_obs,
                               type = "Observed")

      forecast_df <- forecast()$data
      forecast_df$date <- as.character(forecast_df$date)
      if (status() == "Forecast"){
        forecast_df <- forecast_df %>%
          dplyr::filter(location %in% input$loc) %>%
          dplyr::mutate(type = "Forecast") %>%
          dplyr::select(location, date, point, lower, upper, type)
      } else {
        forecast_df <- forecast_df %>%
          dplyr::filter(location %in% input$loc) %>%
          dplyr::mutate(type = "Comparison") %>%
          dplyr::rename(point = outcome()) %>%
          dplyr::select(location, date, point, type)
      }
      df_plot <- bind_rows(observed_df, forecast_df) %>%
        dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d"))
      df_plot
    })

    coverage <- reactive({
      item <- paste0(input$loc, "-cover")
      cover <- scoring()$full_results[[item]]
      cover
    })

    difference <- reactive({
      item <- paste0(input$loc, "-diff")
      diff <- scoring()$full_results[[item]]
      diff
    })

    repeats <- reactive({
      item <- paste0(input$loc, "-repeat")
      df <- scoring()$full_results[[item]]
      df
    })

    taper <- reactive({
      item <- paste0(input$loc, "-taper")
      df <- scoring()$full_results[[item]]
      df
    })

    trend <- reactive({
      item <- paste0(input$loc, "-trend")
      df <- scoring()$full_results[[item]]$output
      df
    })

    plotting <- reactive({

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
      } else if(input$plot_type == "repeat"){
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
    }) %>%
      bindEvent(input$loc, input$plot_type)

    output$plane_plot <- renderPlot({
      plotting()
    })

    output$score_plot <- renderPlot({
      purrr::map_df(scoring()$scores_summary, as_tibble) %>%
        tidyr::separate_rows(components, sep = ";") %>%
        mutate(flagged2 = purrr::map2_chr(.data$components, .data$flagged, function(x,y) if_else(grepl(x, y), "Flagged", "Not Flagged"))) %>%
        ggplot(aes(components, location)) +
        geom_tile(aes(fill = flagged2), col = "black") +
        theme_minimal() +
        theme(panel.grid = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
        scale_fill_manual(values = c("firebrick","lightgrey")) +
        labs(x = NULL, y = NULL)
    })

  })
}


