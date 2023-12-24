#' Include date levels
#'
#' When the dimension is defined as date type, using this function we can select
#' the levels to include in it: day, week, month and year.
#'
#' @param td A `when` object.
#' @param day_level A boolean, include day level.
#' @param week_level A boolean, include week level.
#' @param month_level A boolean, include month level.
#' @param year_level A boolean, include year level.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_date_levels(week_level = FALSE)
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
include_date_levels.when <-
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
