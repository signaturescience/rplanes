# ~~~~~~~~~~~~~~~~~~~~~~~
# UI Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

plotUI <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(div(id = ns("scoring_results"),
                        fluidRow(
                          tags$h3("Overall"),
                          column(width = 4,
                                 tags$div(style="height:50px"),
                                 plotOutput(ns("score_plot"), height = "500px") %>% shinycssloaders::withSpinner(type = 6, size=2, color = "#246479", image.height = "500px", proxy.height = "500px")),
                          column(width = 8,
                                 DT::DTOutput(ns("score_table")))
                        ),
                        fluidRow(
                          tags$h3("Individual Locations and Components"),
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

plotServer <- function(id, scoring, components, data_1, seed, signal_to_eval, btn1, status, locations, outcome, btn2) {
  moduleServer(id, function(input, output, session) {

    observe({
      updatePickerInput(session = session, inputId = "loc", choices = locations())
      choice <- c("Coverage" = "cover", "Difference" = "diff", "Repeat" = "repeat", "Taper" = "taper", "Trend" = "trend", "Shape" = "shape", "Zero" = "zero")
      plot_choice <- choice[choice %in% components()]
      updateAwesomeRadio(session = session, inputId = "plot_type", choices = plot_choice, inline = TRUE, status = "warning")
    })


    # unhide the scoring results after the analyze button has been clicked
    observe({
      shinyjs::toggle(id = "scoring_results", condition = {btn1()})
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
      ) %>%
        DT::formatRound(., columns = "score", digits = 3)
      })

    plot_df <- reactive({
      cut_date <- seed()[[1]]$meta$cut_date
      dates <- unique(as.character(data_1()$date))
      dates <- dates[dates <= cut_date]

      values_obs <- seed()[[as.character(input$loc)]]$all_values

      observed_df <- data.frame(date = dates,
                               point = values_obs,
                               type = "Observed")

      eval_df <- signal_to_eval()$data
      eval_df$date <- as.character(eval_df$date)

      if (status() == "Forecast"){
        eval_df <- eval_df %>%
          dplyr::filter(location %in% input$loc) %>%
          dplyr::select(location, date, point, lower, upper)
      } else {

        ## need to parse cut date from seed to filter evaluation data for observed signal
        cut_date <- purrr::map(seed(), ~.x$meta$cut_date) %>% unlist() %>% as.Date(origin = "1970-01-01") %>% unique(.)
        eval_df <- eval_df %>%
          dplyr::filter(date > cut_date) %>%
          dplyr::filter(location %in% input$loc) %>%
          dplyr::rename(point = outcome()) %>%
          dplyr::select(location, date, point)
      }

      eval_df <-
        eval_df %>%
        mutate(type = "Evaluated")

      df_plot <- bind_rows(observed_df, eval_df) %>%
        dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d"))
      df_plot
    })

    ## parsing the scoring list for each component / location
    coverage <- reactive({
      item <- paste0(input$loc, "-cover")
      res <- scoring()$full_results[[item]]
      res
    })

    difference <- reactive({
      item <- paste0(input$loc, "-diff")
      res <- scoring()$full_results[[item]]
      res
    })

    repeats <- reactive({
      item <- paste0(input$loc, "-repeat")
      res <- scoring()$full_results[[item]]
      res
    })

    taper <- reactive({
      item <- paste0(input$loc, "-taper")
      res <- scoring()$full_results[[item]]
      res
    })

    trend <- reactive({
      item <- paste0(input$loc, "-trend")
      res <- scoring()$full_results[[item]]
      res
    })

    shape <- reactive({
      item <- paste0(input$loc, "-shape")
      res <- scoring()$full_results[[item]]
      res
    })

    zero <- reactive({
      item <- paste0(input$loc, "-zero")
      res <- scoring()$full_results[[item]]
      res
    })

    plotting <- reactive({

      ## conditionally render different plots for each individual component
      if(input$plot_type == "cover"){

        ## get the date of the last observed data to find the last observed value ...
        ## ... and to use in the coverage data to create plot line segment
        last_obs_date <-
          plot_df() %>%
          dplyr::filter(type == "Observed") %>%
          dplyr::pull(date) %>%
          max(.)

        ## get last observed value
        last_obs_val <-
          plot_df() %>%
          dplyr::filter(date == last_obs_date) %>%
          dplyr::pull(point)

        ## find the first evaluated date for coverage data below
        first_eval_date <-
          plot_df() %>%
          dplyr::filter(type == "Evaluated") %>%
          dplyr::pull(date) %>%
          min(.)

        ## create coverage data with two points (last observed, first evaluated)
        cover_dat <-
          plot_df() %>%
          dplyr::filter(date >= last_obs_date & date <= first_eval_date) %>%
          ## determine whether or not flag is raised
          dplyr::mutate(flag = ifelse(type == "Observed" & coverage()$indicator, TRUE, FALSE))

        p <- plot_df() %>%
          ggplot(aes(x = date, y = point)) +
          geom_point(aes(color = type), size = 3) +
          geom_point(data = cover_dat %>% dplyr::filter(flag), aes(date, point, alpha = "Not Covered by PI"), shape = 5, size = 6, stroke=2, color = 'darkred') +
          geom_segment(aes(x = min(cover_dat$date), xend = max(cover_dat$date), y = last_obs_val, yend = last_obs_val), alpha = 0.5, lty = "dotted") +
          geom_ribbon(aes(date, ymin = lower, ymax = upper), alpha = 0.5) +
          geom_line(alpha = 0.3) +
          labs(title = paste0("Coverage", ifelse(coverage()$indicator, " (Flagged)", " (Not Flagged)")), subtitle = paste0("Location: ", input$loc), x = "", y = "Value", caption = "Evaluated points not covered by the first horizon forecast PI are highlighted in red diamond.") +
          theme(legend.title=element_blank())
      } else if(input$plot_type == "diff"){
        df_plot2 <- plot_df() %>%
          dplyr::mutate(diff = point - dplyr::lag(point)) %>%
          dplyr::mutate(flag = diff > difference()$maximum_difference)
        p <- df_plot2 %>%
          ggplot(aes(x = date, y = point)) +
          geom_point(aes(color = type), size = 4) +
          geom_point(data = df_plot2 %>% dplyr::filter(flag == TRUE), aes(date, point, alpha = "Difference"), shape = 5, size = 6, stroke=2, color = 'darkred')  +
          labs(title = paste0("Difference", ifelse(difference()$indicator, " (Flagged)", " (Not Flagged)")),, x = "", y = "Value", subtitle = paste0("Location: ", input$loc), caption = paste0("Flagged point-to-point difference greater than ", difference()$maximum_difference, " is highlighted in red diamond.")) +
          geom_line(alpha = 0.3) +
          theme(legend.title=element_blank())
      } else if(input$plot_type == "repeat"){
        repeat_tbl <- repeats()$repeats
        p <- ggplot() +
          geom_point(data = plot_df(), aes(date, point, color = type), size = 4) +
          geom_point(data = repeat_tbl, aes(date, point, alpha = "Repeat"), shape = 5, size = 6, stroke=2, color = 'darkred') +
          geom_line(data = plot_df(), aes(date, point), alpha = 0.3) +
          labs(title = paste0("Repeat", ifelse(repeats()$indicator, " (Flagged)", " (Not Flagged)")), x = "", y = "Value", subtitle = paste0("Location: ", input$loc), caption = "Flagged repeats that exceed the tolerance are highlighted in red diamond.") +
          theme(legend.title=element_blank())
      } else if(input$plot_type == "taper"){
        p <- plot_df() %>%
          ggplot(aes(x = date, y = point)) +
          geom_point(aes(color = type), size = 4) +
          geom_ribbon(aes(date, ymin = lower, ymax = upper), alpha = 0.5) +
          geom_line(alpha = 0.3) +
          labs(title = paste0("Taper", ifelse(taper()$indicator, " (Flagged)", " (Not Flagged)")), x = "", y = "Value", subtitle = paste0("Location: ", input$loc)) +
          theme(legend.title=element_blank())
      } else if (input$plot_type == "trend") {

        ## get the output data tibble from trend for change points below
        trend_df <- trend()$output

        p <-
          plot_df() %>%
          ggplot(aes(x = date, y = point)) +
          geom_point(aes(color = type), size = 4) +
          geom_point(data = trend_df %>% dplyr::filter(Flagged) %>% dplyr::rename(date = Date, point = Value), aes(date, point, alpha = "Trend Change Point"), shape = 5, size = 6, stroke=2, color = 'darkred') +
          geom_line(alpha = 0.3) +
          labs(title = paste0("Trend", ifelse(trend()$indicator, " (Flagged)", " (Not Flagged)")), x = "", y = "Value", subtitle = paste0("Location: ", input$loc), caption = paste0("Using a significance of ", input$sig, ".\nFlagged change points are highlighted in red diamond.")) +
          theme(legend.title=element_blank())
      } else if (input$plot_type == "shape") {

        shape_df <-
          plot_df() %>%
          dplyr::filter(type == "Evaluated") %>%
          dplyr::mutate(flag = ifelse(shape()$indicator, TRUE, FALSE))


        p <- ggplot() +
          geom_point(data = plot_df(), aes(date, point, color = type), size = 4) +
          geom_point(data = shape_df %>% dplyr::filter(flag), aes(date, point, alpha = "Shape"), shape = 5, size = 6, stroke=2, color = 'darkred') +
          geom_line(data = plot_df(), aes(date, point), alpha = 0.3) +
          labs(title = paste0("Shape", ifelse(shape()$indicator, " (Flagged)", " (Not Flagged)")), x = "", y = "Value", subtitle = paste0("Location: ", input$loc), caption = "Flagged shapes are highlighted in red diamond.") +
          theme(legend.title=element_blank())

      } else if (input$plot_type == "zero") {

        ## if there is a zero flagged then find the zero
        ## otherwise create an empty tibble so we dont have change code in plotting
        if(zero()$indicator) {
          zero_df <-
            plot_df() %>%
            dplyr::filter(type == "Evaluated") %>%
            dplyr::mutate(flag = ifelse(zero()$indicator, TRUE, FALSE)) %>%
            dplyr::filter(value == 0)
        } else {
          zero_df <-
            plot_df() %>%
            dplyr::filter(type == "Evaluated") %>%
            dplyr::mutate(flag = ifelse(zero()$indicator, TRUE, FALSE))
        }

        p <- ggplot() +
          geom_point(data = plot_df(), aes(date, point, color = type), size = 4) +
          geom_point(data = zero_df %>% dplyr::filter(flag), aes(date, point, alpha = "Zero"), shape = 5, size = 6, stroke=2, color = 'darkred') +
          geom_line(data = plot_df(), aes(date, point), alpha = 0.3) +
          labs(title = paste0("Zero", ifelse(zero()$indicator, " (Flagged)", " (Not Flagged)")), x = "", y = "Value", subtitle = paste0("Location: ", input$loc), caption = "Flagged zeros are highlighted in red diamond.") +
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


