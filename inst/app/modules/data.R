# ~~~~~~~~~~~~~~~~~~~~~~~
# UI Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

dataUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h3(HTML("<span style='color: #008B8B;'>Observed Data Used in Seed</span>")),
      fluidRow(column(width = 9), DT::dataTableOutput(ns("observed")))

      ),
    wellPanel(
      h3(HTML("<span style='color: #008B8B;'>Data Set Evaluated</span>")),
      fluidRow(column(width = 9), DT::dataTableOutput(ns("evaluated")))
  )
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~
# Server Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

dataServer <- function(id, btn1, data_1, data_2, signal_type, n_obs_eval) {
  moduleServer(id, function(input, output, session) {


    output$observed <- DT::renderDataTable(server = FALSE,{

      ## conditional logic to handle observed signal and cut date if needed
      if(signal_type == "Observed") {
        cut_date <- min(tail(data_1()$date,n_obs_eval))
        dat <- data_1() %>%
          dplyr::filter(date < cut_date)
      } else {
        dat <- data_1()
      }

      DT::datatable(dat, extensions = "Buttons",
                    filter = "top",
                    selection = "none", #this is to avoid select rows if you click on the rows
                    rownames = FALSE,
                    options = list(
                      scrollX = TRUE,
                      autoWidth = FALSE,
                      dom = 'lrtip',
                      # add the option to display more rows as a length menu
                      lengthMenu = list(c(10, 30, 50, -1),
                                        c('10', '30', '50', 'All'))
                    ),
                    class = "display")
    })

    output$evaluated <- DT::renderDataTable(server = FALSE,{

      ## conditional logic to handle observed signal and cut date if needed
      if(signal_type == "Observed") {
        cut_date <- min(tail(data_1()$date,n_obs_eval))
        dat <- data_1() %>%
          dplyr::filter(date >= cut_date)
      } else {
        dat <- data_2()
      }
      DT::datatable(dat, extensions = "Buttons",
                    filter = "top",
                    selection = "none", #this is to avoid select rows if you click on the rows
                    rownames = FALSE,
                    options = list(
                      scrollX = TRUE,
                      autoWidth = FALSE,
                      dom = 'lrtip',
                      # add the option to display more rows as a length menu
                      lengthMenu = list(c(10, 30, 50, -1),
                                        c('10', '30', '50', 'All'))
                    ),
                    class = "display")
    })
  })
}
