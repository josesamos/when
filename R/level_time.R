#' Include time
#'
#' Include time.
#'
#' @param td A `when` object.
#' @param include_time A boolean, include a field for the time.
#' @param include_minute A boolean, include the minute level of detail.
#' @param include_second A boolean, include the second level of detail.
#' @param include_day_part A boolean, include the parts of the day.
#'
#' @return A `when` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_time_level()
#'
#' @export
include_time_level <-
  function(td,
           include_time,
           include_minute,
           include_second,
           include_day_part)
    UseMethod("include_time_level")

#' @rdname include_time_level
#'
#' @export
include_time_level.when <-
  function(td,
           include_time = TRUE,
           include_minute = TRUE,
           include_second = TRUE,
           include_day_part) {
    stopifnot("'include_time' must be of logical type." = is.logical(include_time))
    stopifnot("'include_minute' must be of logical type." = is.logical(include_minute))
    stopifnot("'include_second' must be of logical type." = is.logical(include_second))
    stopifnot("'include_day_part' must be of logical type." = is.logical(include_day_part))
    td$include_time <- include_time
    td$include_minute <- include_minute
    td$include_second <- include_second
    if (td$include_second) {
      td$include_minute <- TRUE
    }
    td$include_day_part <- include_day_part
    td
  }
