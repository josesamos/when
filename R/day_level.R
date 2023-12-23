#' Include day level
#'
#' Include day level.
#'
#' @param td A `timedimension` object.
#' @param day_level A boolean, include day level.
#' @param include_date A boolean, include the date.
#' @param include_month_day A boolean, include the day number in the month.
#' @param include_week_day A boolean, the day number in the week.
#' @param include_day_name A boolean, include the name of the day of the week.
#' @param include_day_abbr A boolean, include the name of the day of the week in
#' abbreviated version.
#' @param include_day_num_name A boolean, include the number and name of the day
#' of the week.
#' @param include_day_num_abbr A boolean, include the number and name of the day
#' of the week in abbreviated version.
#' @param include_quarter_day A boolean, include the number of the day in the quarter.
#' @param include_year_day A boolean, include the number of the day in the year.
#'
#' @return A `timedimension` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- timedimension() |>
#'   include_day_level()
#'
#' @export
include_day_level <- function(td,
                                day_level,
                                include_date,
                                include_month_day,
                                include_week_day,
                                include_day_name,
                                include_day_abbr,
                                include_day_num_name,
                                include_day_num_abbr,
                                include_quarter_day,
                                include_year_day) UseMethod("include_day_level")

#' @rdname include_day_level
#'
#' @export
include_day_level.timedimension <- function(td,
                                              day_level = TRUE,
                                              include_date = TRUE,
                                              include_month_day = TRUE,
                                              include_week_day = TRUE,
                                              include_day_name = TRUE,
                                              include_day_abbr = TRUE,
                                              include_day_num_name = TRUE,
                                              include_day_num_abbr = TRUE,
                                              include_quarter_day = TRUE,
                                              include_year_day = TRUE) {
  stopifnot("'day_level' must be of logical type." = is.logical(day_level))
  stopifnot("'include_date' must be of logical type." = is.logical(include_date))
  stopifnot("'include_month_day' must be of logical type." = is.logical(include_month_day))
  stopifnot("'include_week_day' must be of logical type." = is.logical(include_week_day))
  stopifnot("'include_day_name' must be of logical type." = is.logical(include_day_name))
  stopifnot("'include_day_abbr' must be of logical type." = is.logical(include_day_abbr))
  stopifnot("'include_day_num_name' must be of logical type." = is.logical(include_day_num_name))
  stopifnot("'include_day_num_abbr' must be of logical type." = is.logical(include_day_num_abbr))
  stopifnot("'include_quarter_day' must be of logical type." = is.logical(include_quarter_day))
  stopifnot("'include_year_day' must be of logical type." = is.logical(include_year_day))
  td$day_level <- day_level
  td$include_date <- include_date
  td$include_month_day <- include_month_day
  td$include_week_day <- include_week_day
  td$include_day_name <- include_day_name
  td$include_day_abbr <- include_day_abbr
  td$include_day_num_name <- include_day_num_name
  td$include_day_num_abbr <- include_day_num_abbr
  td$include_quarter_day <- include_quarter_day
  td$include_year_day <- include_year_day
  td
}

