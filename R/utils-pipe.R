#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Month subtraction
#'
#' See \code{lubridate::\link[lubridate:add_with_rollback]{\%m-\%}} for details.
#'
#' @name %m-%
#' @rdname month-subtraction
#' @keywords internal
#' @export
#' @importFrom lubridate %m-%
#' @param e1 A period or a date-time object of class [POSIXlt], [POSIXct]
#' or [Date].
#' @param e2 A period or a date-time object of class [POSIXlt], [POSIXct]
#' or [Date]. Note that one of e1 and e2 must be a period and the other a
#' date-time object.
#' @return  A date-time object of class POSIXlt, POSIXct or Date
NULL
