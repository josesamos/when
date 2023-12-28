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


#' Define instances
#'
#' Using this function we can define the instances from which the dimension will
#' be generated according to the rest of its defined characteristics.
#'
#' We must indicate dates or date components in ISO 8601 format (yyyy-mm-dd). The
#' times in hh:mm:ss format.
#'
#' @param td A `when` object.
#' @param start A string, start of the period to be included in the dimension.
#' @param end A string, end of the period to be included in the dimension.
#' @param values A vector of string.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td_1 <- when() |>
#'   define_instances(start = "2020", end = "2030")
#'
#' td_1 <- when() |>
#'   define_instances(start = "2020-01-01", end = "2030-01-01")
#'
#' td_2 <- when(type = 'time') |>
#'   define_instances(values = 1:5)
#'
#' @export
define_instances <-
  function(td, start, end, values)
    UseMethod("define_instances")

#' @rdname define_instances
#'
#' @export
define_instances.when <-
  function(td,
           start = NULL,
           end = NULL,
           values = NULL) {
    if (is.null(start) & is.null(end) & !is.null(td$start) & !is.null(td$end)) {
      td$start <- NULL
      td$end <- NULL
    }
    if (is.null(values) & !is.null(td$values)) {
      td$values <- NULL
    }
    td <- validate_start_end(td, start, end)
    td <- validate_values(td, values)
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
    td$level_include_conf[names(td$level_type[td$level_type == 'date'])] <- FALSE
    td$level_include_conf[names(td$level_type[td$level_type == 'time'])] <- TRUE
    if (td$type != 'time') {
      td$start = lubridate::today()
      td$end = lubridate::today()
    }
  } else {
    td$level_include_conf[names(td$level_type[td$level_type == 'time'])] <- FALSE
    if (!any(td$level_include_conf[names(td$level_type[td$level_type == 'date'])])) {
      td$level_include_conf[names(td$level_type[td$level_type == 'date'])] <- TRUE
    }
  }
  td$type <- type
  td
}

