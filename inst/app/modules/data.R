# ~~~~~~~~~~~~~~~~~~~~~~~
# UI Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

dataUI <- function(id){
  ns <- NS(id)
  tagList(
    card(
      full_screen = TRUE,
      card_header("Observed Dataset"),
      DT::DTOutput(ns("observed"))
    ),
    card(
      full_screen = TRUE,
      card_header("Forecast Dataset"),
      DT::DTOutput(ns("forecast"))
    )
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~
# Server Side ####
#~~~~~~~~~~~~~~~~~~~~~~~~

dataServer <- function(id, data_1, data_2) {
  moduleServer(id, function(input, output, session) {

    output$observed <- DT::renderDT({
      data_1() %>% DT::datatable(rownames = F, filter = "top", escape = F, height = "100%", fillContainer = T, extensions =c("Buttons"),
                                 options = list(dom = 'lBrtip',
                                                pageLength = 10,
                                                lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All")),
                                                buttons = list('copy', list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download"))
                                                ))
    })

    output$forecast <- DT::renderDT({
      data_2() %>% DT::datatable(rownames = F, filter = "top", escape = F, height = "100%", fillContainer = T, extensions =c("Buttons"),
                                 options = list(dom = 'lBrtip',
                                                buttons = list('copy', list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download")),
                                                pageLength = 10,
                                                lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All"))))
    })
  })
}
