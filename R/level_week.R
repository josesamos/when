#' Include week level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the week level and its attributes to include in it: week, year_week and week_date.
#'
#' For the first and last days of the year, the year associated with the week may
#' be different from the year of the date, depending on the date type selected.
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
#' @param week_level A boolean, include week level.
#' @param include_week A boolean, include the week number.
#' @param include_year_week A boolean, include the year-week combination.
#' @param include_week_date A boolean, include the date from which the week is
#' obtained.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_week_level(include_week_date = FALSE)
#'
#' @export
include_week_level <-
  function(td, week_level, include_week, include_year_week, include_week_date)
    UseMethod("include_week_level")

#' @rdname include_week_level
#'
#' @export
include_week_level.when <-
  function(td,
           week_level = TRUE,
           include_week = TRUE,
           include_year_week = TRUE,
           include_week_date = TRUE) {
    stopifnot("'week_level' must be of logical type." = is.logical(week_level))
    stopifnot("'include_week' must be of logical type." = is.logical(include_week))
    stopifnot("'include_year_week' must be of logical type." = is.logical(include_year_week))
    stopifnot("'include_week_date' must be of logical type." = is.logical(include_week_date))
    td$week_level <- week_level
    td$include_week <- include_week
    td$include_year_week <- include_year_week
    td$include_week_date <- include_week_date
    td
  }
