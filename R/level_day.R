#' Include day level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the day level and its attributes to include in it: date, month_day, week_day,
#' quarter_day and year_day.
#'
#' For the week_day we have the day number, its name and the name abbreviation.
#' So that the order of the names corresponds to the alphabetical order, the
#' combination of day number and name and/or abbreviation is included.
#'
#' @param td A `when` object.
#' @param level A boolean, include day level.
#' @param include_all A boolean, include all fields of the level.
#' @param date A boolean, include the date.
#' @param month_day A boolean, include the day number in the month.
#' @param week_day A boolean, the day number in the week.
#' @param day_name A boolean, include the name of the day of the week.
#' @param day_abbr A boolean, include the name of the day of the week in
#' abbreviated version.
#' @param day_num_name A boolean, include the number and name of the day
#' of the week.
#' @param day_num_abbr A boolean, include the number and name of the day
#' of the week in abbreviated version.
#' @param quarter_day A boolean, include the number of the day in the quarter.
#' @param year_day A boolean, include the number of the day in the year.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_day_level(day_abbr = FALSE,
#'                     day_num_abbr = FALSE)
#'
#' @export
include_day_level <- function(td,
                              level,
                              include_all,
                              date,
                              month_day,
                              week_day,
                              day_name,
                              day_abbr,
                              day_num_name,
                              day_num_abbr,
                              quarter_day,
                              year_day)
  UseMethod("include_day_level")

#' @rdname include_day_level
#'
#' @export
include_day_level.when <- function(td,
                                   level = TRUE,
                                   include_all = FALSE,
                                   date = TRUE,
                                   month_day = TRUE,
                                   week_day = TRUE,
                                   day_name = TRUE,
                                   day_abbr = FALSE,
                                   day_num_name = TRUE,
                                   day_num_abbr = FALSE,
                                   quarter_day = FALSE,
                                   year_day = FALSE) {
  stopifnot("'level' must be of logical type." = is.logical(level))
  stopifnot("'include_all' must be of logical type." = is.logical(include_all))
  stopifnot("'date' must be of logical type." = is.logical(date))
  stopifnot("'month_day' must be of logical type." = is.logical(month_day))
  stopifnot("'week_day' must be of logical type." = is.logical(week_day))
  stopifnot("'day_name' must be of logical type." = is.logical(day_name))
  stopifnot("'day_abbr' must be of logical type." = is.logical(day_abbr))
  stopifnot("'day_num_name' must be of logical type." = is.logical(day_num_name))
  stopifnot("'day_num_abbr' must be of logical type." = is.logical(day_num_abbr))
  stopifnot("'quarter_day' must be of logical type." = is.logical(quarter_day))
  stopifnot("'year_day' must be of logical type." = is.logical(year_day))
  td$day_level <- level
  if (include_all) {
    td$include_date <- TRUE
    td$include_month_day <- TRUE
    td$include_week_day <- TRUE
    td$include_day_name <- TRUE
    td$include_day_abbr <- TRUE
    td$include_day_num_name <- TRUE
    td$include_day_num_abbr <- TRUE
    td$include_quarter_day <- TRUE
    td$include_year_day <- TRUE
  } else {
    td$include_date <- date
    td$include_month_day <- month_day
    td$include_week_day <- week_day
    td$include_day_name <- day_name
    td$include_day_abbr <- day_abbr
    td$include_day_num_name <- day_num_name
    td$include_day_num_abbr <- day_num_abbr
    td$include_quarter_day <- quarter_day
    td$include_year_day <- year_day
  }
  td
}
