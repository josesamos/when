#' Include month level
#'
#' Include month level.
#'
#' @param td A `timedimension` object.
#' @param month_level A boolean, include month level.
#' @param include_month A boolean, include the month number.
#' @param include_year_month A boolean, include the year-month combination.
#' @param include_month_name A boolean, include the month name.
#' @param include_month_abbr A boolean, include the month name abbreviated version.
#' @param include_month_num_name A boolean, include the month number and name.
#' @param include_month_num_abbr A boolean, include the month number and name
#' abbreviated version.
#' @param include_quarter A boolean, include the quarter field.
#' @param include_year_quarter A boolean, include the quarter field.
#' @param include_semester A boolean, include the semester field.
#' @param include_year_semester A boolean, include the semester field.
#'
#' @return A `timedimension` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- timedimension() |>
#'   include_month_level()
#'
#' @export
include_month_level <-
  function(td,
           month_level,
           include_month,
           include_year_month,
           include_month_name,
           include_month_abbr,
           include_month_num_name,
           include_month_num_abbr,
           include_quarter,
           include_year_quarter,
           include_semester,
           include_year_semester)
    UseMethod("include_month_level")

#' @rdname include_month_level
#'
#' @export
include_month_level.timedimension <-
  function(td,
           month_level = TRUE,
           include_month = TRUE,
           include_year_month = TRUE,
           include_month_name = TRUE,
           include_month_abbr = FALSE,
           include_month_num_name = TRUE,
           include_month_num_abbr = FALSE,
           include_quarter = FALSE,
           include_year_quarter = FALSE,
           include_semester = FALSE,
           include_year_semester = FALSE) {
    stopifnot("'month_level' must be of logical type." = is.logical(month_level))
    stopifnot("'include_month' must be of logical type." = is.logical(include_month))
    stopifnot("'include_year_month' must be of logical type." = is.logical(include_year_month))
    stopifnot("'include_month_name' must be of logical type." = is.logical(include_month_name))
    stopifnot("'include_month_abbr' must be of logical type." = is.logical(include_month_abbr))
    stopifnot("'include_month_num_name' must be of logical type." = is.logical(include_month_num_name))
    stopifnot("'include_month_num_abbr' must be of logical type." = is.logical(include_month_num_abbr))
    stopifnot("'include_quarter' must be of logical type." = is.logical(include_quarter))
    stopifnot("'include_year_quarter' must be of logical type." = is.logical(include_year_quarter))
    stopifnot("'include_semester' must be of logical type." = is.logical(include_semester))
    stopifnot("'include_year_semester' must be of logical type." = is.logical(include_year_semester))
    td$month_level <- month_level
    td$include_month <- include_month
    td$include_year_month <- include_year_month
    td$include_month_name <- include_month_name
    td$include_month_abbr <- include_month_abbr
    td$include_month_num_name <- include_month_num_name
    td$include_month_num_abbr <- include_month_num_abbr
    td$include_quarter <- include_quarter
    td$include_year_quarter <- include_year_quarter
    td$include_semester <- include_semester
    td$include_year_semester <- include_year_semester
    td
  }
