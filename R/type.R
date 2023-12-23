#' Configure type
#'
#' Configure type.
#'
#' @param td A `when` object.
#' @param type A string, type of calendar (NULL, 'iso', 'epi' or 'time').
#' @param locale A locale, to use for day and month names.
#' @param week_starts_monday A boolean.
#'
#' @return A `when` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- when() |>
#'   configure_type()
#'
#' @export
configure_type <-
  function(td, type, locale, week_starts_monday)
    UseMethod("configure_type")

#' @rdname configure_type
#'
#' @export
configure_type.when <- function(td,
                                         type = NULL,
                                         locale = Sys.getlocale("LC_TIME"),
                                         week_starts_monday = TRUE) {
  td <- validate_type(td, type)
  stopifnot("'week_starts_monday' must be of logical type." = is.logical(week_starts_monday))
  td$locale <- locale
  td$week_starts_monday <- week_starts_monday
  td
}

#' Validate type parameter
#'
#' @param td A `when` object.
#' @param type A string, type of calendar (NULL, 'iso', 'epi' or 'time').
#'
#' @return A `when` object.
#'
#' @keywords internal
validate_type <- function(td, type) {
  if (!is.null(type)) {
    type <- snakecase::to_snake_case(type)
    stopifnot("'type' does not have one of the allowed values." = type %in% c('iso', 'epi', 'time'))
  } else {
    type <- 'std'
  }
  if (type == 'time') {
    td$day_level <- FALSE
    td$week_level <- FALSE
    td$month_level <- FALSE
    td$year_level <- FALSE
    td$hour_level <- TRUE
  } else {
    td$hour_level <- FALSE
  }
  td$type <- type
  td
}
