#' Configure dimension
#'
#' With this function we can define the characteristics of the dimension that do
#' not depend on the levels it includes, such as the name, type, location or the
#' day the week begins.
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
#'   configure_dimension(name = 'when', type = 'time')
#'
#' @export
configure_dimension <-
  function(td, name, type, locale, week_starts_monday)
    UseMethod("configure_dimension")

#' @rdname configure_dimension
#'
#' @export
configure_dimension.when <- function(td,
                                name = 'when',
                                type = NULL,
                                locale = Sys.getlocale("LC_TIME"),
                                week_starts_monday = TRUE) {
  if (is.null(name) & is.null(td$table_name)) {
    td$table_name = 'when'
  } else {
    stopifnot("'name' must have a single value." = length(name) == 1)
    td$table_name = name
  }
  td <- validate_type(td, type)
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
  }
  td$type <- type
  td
}
