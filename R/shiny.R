#' @title `rplanes` explorer app launcher
#'
#' @description
#'
#' The `rplanes` explorer app allows a user to interactively upload their own data (or view an internal example) to explore the plausibility analysis functionality.
#'
#' @param ... Additional arguments to be passed to [shiny::runApp]
#'
#' @return This function operates as a side-effect and starts the `rplanes` Shiny app.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the explorer app
#' rplanes_launcher(host = "0.0.0.0",
#'                  launch.browser = TRUE,
#'                  port = 80)
#' }
rplanes_launcher <- function(...){
  shiny::runApp(appDir = system.file("app", package="rplanes"), ... )
}
