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
#' @param day_level A boolean, include day level.
#' @param include_all A boolean, include all fields of the level.
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
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_day_level(include_day_abbr = FALSE,
#'                     include_day_num_abbr = FALSE)
#'
#' @export
include_day_level <- function(td,
                              day_level,
                              include_all,
                              include_date,
                              include_month_day,
                              include_week_day,
                              include_day_name,
                              include_day_abbr,
                              include_day_num_name,
                              include_day_num_abbr,
                              include_quarter_day,
                              include_year_day)
  UseMethod("include_day_level")

#' @rdname include_day_level
#'
#' @export
include_day_level.when <- function(td,
                                   day_level = TRUE,
                                   include_all = FALSE,
                                   include_date = TRUE,
                                   include_month_day = TRUE,
                                   include_week_day = TRUE,
                                   include_day_name = TRUE,
                                   include_day_abbr = FALSE,
                                   include_day_num_name = TRUE,
                                   include_day_num_abbr = FALSE,
                                   include_quarter_day = FALSE,
                                   include_year_day = FALSE) {
  stopifnot("'day_level' must be of logical type." = is.logical(day_level))
  stopifnot("'include_all' must be of logical type." = is.logical(include_all))
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
    td$include_date <- include_date
    td$include_month_day <- include_month_day
    td$include_week_day <- include_week_day
    td$include_day_name <- include_day_name
    td$include_day_abbr <- include_day_abbr
    td$include_day_num_name <- include_day_num_name
    td$include_day_num_abbr <- include_day_num_abbr
    td$include_quarter_day <- include_quarter_day
    td$include_year_day <- include_year_day
  }
  td
}
