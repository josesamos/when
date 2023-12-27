#' Configure dimension
#'
#' With this function we can define the characteristics of the dimension that do
#' not depend on the levels it includes, such as the name, type, location or the
#' day the week begins.
#'
#' The `week_starts_monday` parameter only affects the numbering of days, not weeks.
#'
#' The week number associated with each date depends on the type of date dimension
#' selected: standard (NULL), ISO 8601 ('iso') or epidemiological ('epi').
#'
#' The standard week numbers blocks of 7 days beginning on January 1. The last week
#' of the year can be less than 7 days long.
#'
#' The ISO 8601 week numbers blocks of 7 days from Monday to Sunday. The first and
#' last week of the year can contain days from the previous or next year.
#'
#' The epidemiological week is like ISO 8601 only that it considers that the week
#' begins on Sunday, it numbers blocks from Sunday to Monday.
#'
#' @param td A `when` object.
#' @param name A string, table name.
#' @param type A string, type of calendar (NULL, 'iso', 'epi' or 'time').
#' @param locale A locale, to use for day and month names.
#' @param week_starts_monday A boolean.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   configure_dimension(name = 'time', type = 'time')
#'
#' @export
configure_dimension <-
  function(td, name, type, locale, week_starts_monday)
    UseMethod("configure_dimension")

#' @rdname configure_dimension
#'
#' @export
configure_dimension.when <- function(td,
                                name = NULL,
                                type = NULL,
                                locale = Sys.getlocale("LC_TIME"),
                                week_starts_monday = TRUE) {
  if (!is.null(name)) {
    stopifnot("'name' must have a single value." = length(name) == 1)
    td$table_name = name
  }
  td <- validate_type(td, type)
  td <- validate_start_end(td, td$start, td$end)
  td <- validate_values(td, td$values)
  stopifnot("'week_starts_monday' must be of logical type." = is.logical(week_starts_monday))
  td$week_starts_monday <- week_starts_monday
  td$locale <- locale
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
    td$time_level <- TRUE
  } else {
    td$time_level <- FALSE
    if (!(td$day_level | td$week_level | td$month_level | td$year_level)) {
      td$day_level <- TRUE
      td$week_level <- TRUE
      td$month_level <- TRUE
      td$year_level <- TRUE
    }
  }
  td$type <- type
  td
}
