
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_observed <- function(x) {
  all(class(x) == c("signal","observed"))
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_forecast <- function(x) {
  all(class(x) == c("signal","forecast"))
}
