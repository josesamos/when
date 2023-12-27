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
#' @param level A boolean, include week level.
#' @param include_all A boolean, include all fields of the level.
#' @param week A boolean, include the week number.
#' @param year_week A boolean, include the year-week combination.
#' @param week_date A boolean, include the date from which the week is
#' obtained.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_week_level(week_date = FALSE)
#'
#' @export
include_week_level <-
  function(td,
           level,
           include_all,
           week,
           year_week,
           week_date)
    UseMethod("include_week_level")

#' @rdname include_week_level
#'
#' @export
include_week_level.when <-
  function(td,
           level = TRUE,
           include_all = FALSE,
           week = TRUE,
           year_week = TRUE,
           week_date = FALSE) {
    stopifnot("'level' must be of logical type." = is.logical(level))
    stopifnot("'include_all' must be of logical type." = is.logical(include_all))
    stopifnot("'week' must be of logical type." = is.logical(week))
    stopifnot("'year_week' must be of logical type." = is.logical(year_week))
    stopifnot("'week_date' must be of logical type." = is.logical(week_date))
    td$week_level <- level
    if (include_all) {
      td$include_week <- TRUE
      td$include_year_week <- TRUE
      td$include_week_date <- TRUE
    } else {
      td$include_week <- week
      td$include_year_week <- year_week
      td$include_week_date <- week_date
    }
    td
  }
