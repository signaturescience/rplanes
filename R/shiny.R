#' @title RPLANES explorer app launcher
#'
#' @description
#'
#' The Rplanes explorer app allows a user to interactively use their own data sets, or an internal example data set, to explore the various functions developed for plausibility analysis.
#'
#' @param ... Additional arguments to be passed to [shiny::runApp]
#'
#' @return This function starts the Rplanes Shiny app.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the explorer app
#' app_launcher(host = "0.0.0.0",
#'              launch.browser = TRUE,
#'              port = 80)
#' }
app_launcher <- function(...){
  shiny::runApp(appDir = system.file("app", package="rplanes"), ... )
}
