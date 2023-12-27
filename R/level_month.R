#' Include month level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the month level and its attributes to include in it: month, quarter and semester.
#'
#' We can also obtain the combination of the year with the mentioned attributes.
#'
#' For the month we have the month number in the year, its name and the abbreviation
#' of the name. So that the order of the names corresponds to the alphabetical order,
#' the combination of month number and name and/or abbreviation is included.
#'
#' @param td A `when` object.
#' @param level A boolean, include month level.
#' @param include_all A boolean, include all fields of the level.
#' @param month A boolean, include the month number.
#' @param year_month A boolean, include the year-month combination.
#' @param month_name A boolean, include the month name.
#' @param month_abbr A boolean, include the month name abbreviated version.
#' @param month_num_name A boolean, include the month number and name.
#' @param month_num_abbr A boolean, include the month number and name
#' abbreviated version.
#' @param quarter A boolean, include the quarter field.
#' @param year_quarter A boolean, include the quarter field.
#' @param semester A boolean, include the semester field.
#' @param year_semester A boolean, include the semester field.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_month_level(month_abbr = FALSE,
#'                       month_num_abbr = FALSE)
#'
#' @export
include_month_level <-
  function(td,
           level,
           include_all,
           month,
           year_month,
           month_name,
           month_abbr,
           month_num_name,
           month_num_abbr,
           quarter,
           year_quarter,
           semester,
           year_semester)
UseMethod("include_month_level")

#' @rdname include_month_level
#'
#' @export
include_month_level.when <-
  function(td,
           level = TRUE,
           include_all = FALSE,
           month = TRUE,
           year_month = TRUE,
           month_name = TRUE,
           month_abbr = FALSE,
           month_num_name = TRUE,
           month_num_abbr = FALSE,
           quarter = FALSE,
           year_quarter = FALSE,
           semester = FALSE,
           year_semester = FALSE) {
    stopifnot("'level' must be of logical type." = is.logical(level))
    stopifnot("'include_all' must be of logical type." = is.logical(include_all))
    stopifnot("'month' must be of logical type." = is.logical(month))
    stopifnot("'year_month' must be of logical type." = is.logical(year_month))
    stopifnot("'month_name' must be of logical type." = is.logical(month_name))
    stopifnot("'month_abbr' must be of logical type." = is.logical(month_abbr))
    stopifnot("'month_num_name' must be of logical type." = is.logical(month_num_name))
    stopifnot("'month_num_abbr' must be of logical type." = is.logical(month_num_abbr))
    stopifnot("'quarter' must be of logical type." = is.logical(quarter))
    stopifnot("'year_quarter' must be of logical type." = is.logical(year_quarter))
    stopifnot("'semester' must be of logical type." = is.logical(semester))
    stopifnot("'year_semester' must be of logical type." = is.logical(year_semester))
    td$month_level <- level
    if (include_all) {
      td$include_month <- TRUE
      td$include_year_month <- TRUE
      td$include_month_name <- TRUE
      td$include_month_abbr <- TRUE
      td$include_month_num_name <- TRUE
      td$include_month_num_abbr <- TRUE
      td$include_quarter <- TRUE
      td$include_year_quarter <- TRUE
      td$include_semester <- TRUE
      td$include_year_semester <- TRUE
    } else {
      td$include_month <- month
      td$include_year_month <- year_month
      td$include_month_name <- month_name
      td$include_month_abbr <- month_abbr
      td$include_month_num_name <- month_num_name
      td$include_month_num_abbr <- month_num_abbr
      td$include_quarter <- quarter
      td$include_year_quarter <- year_quarter
      td$include_semester <- semester
      td$include_year_semester <- year_semester
    }
    td
  }
