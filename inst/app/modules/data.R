# ~~~~~~~~~~~~~~~~~~~~~~~
# UI Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

dataUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(column(width = 9), DT::dataTableOutput(ns("observed")))

      ),
    wellPanel(
      fluidRow(column(width = 9, DT::dataTableOutput(ns("forecast"))))
  )
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~
# Server Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

dataServer <- function(id, data_1, data_2) {
  moduleServer(id, function(input, output, session) {

    output$observed <- DT::renderDataTable(server = FALSE,{
      DT::datatable(data_1(), extensions = "Buttons",
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
                    class = "display")
    })

    output$forecast <- DT::renderDataTable(server = FALSE,{
      DT::datatable(data_2(), extensions = "Buttons",
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
                    class = "display")
    })
  })
}
