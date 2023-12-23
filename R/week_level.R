#' Include week level
#'
#' Include week level.
#'
#' @param td A `timedimension` object.
#' @param week_level A boolean, include week level.
#' @param include_week A boolean, include the week number.
#' @param include_year_week A boolean, include the year-week combination.
#'
#' @return A `timedimension` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- timedimension() |>
#'   include_week_level()
#'
#' @export
include_week_level <-
  function(td, week_level, include_week, include_year_week)
    UseMethod("include_week_level")

#' @rdname include_week_level
#'
#' @export
include_week_level.timedimension <-
  function(td,
           week_level = TRUE,
           include_week = TRUE,
           include_year_week = TRUE) {
    stopifnot("'week_level' must be of logical type." = is.logical(week_level))
    stopifnot("'include_week' must be of logical type." = is.logical(include_week))
    stopifnot("'include_year_week' must be of logical type." = is.logical(include_year_week))
    td$week_level <- week_level
    td$include_week <- include_week
    td$include_year_week <- include_year_week
    td
  }
