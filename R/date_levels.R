#' Include date levels
#'
#' Include date levels.
#'
#' @param td A `timedimension` object.
#' @param day_level A boolean, include day level.
#' @param week_level A boolean, include week level.
#' @param month_level A boolean, include month level.
#' @param year_level A boolean, include year level.
#'
#' @return A `timedimension` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- timedimension() |>
#'   include_date_levels()
#'
#' @export
include_date_levels <-
  function(td,
           day_level,
           week_level,
           month_level,
           year_level)
    UseMethod("include_date_levels")

#' @rdname include_date_levels
#'
#' @export
include_date_levels.timedimension <-
  function(td,
           day_level = TRUE,
           week_level = TRUE,
           month_level = TRUE,
           year_level = TRUE) {
    stopifnot("'day_level' must be of logical type." = is.logical(day_level))
    stopifnot("'week_level' must be of logical type." = is.logical(week_level))
    stopifnot("'month_level' must be of logical type." = is.logical(month_level))
    stopifnot("'year_level' must be of logical type." = is.logical(year_level))
    td$day_level <- day_level
    td$week_level <- week_level
    td$month_level <- month_level
    td$year_level <- year_level
    td
  }
