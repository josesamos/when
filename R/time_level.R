#' Include time
#'
#' Include time.
#'
#' @param td A `timedimension` object.
#' @param include_time A boolean, include a field for the time.
#' @param include_minute A boolean, include the minute level of detail.
#' @param include_second A boolean, include the second level of detail.
#'
#' @return A `timedimension` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- timedimension() |>
#'   include_time_level()
#'
#' @export
include_time_level <-
  function(td,
           include_time,
           include_minute,
           include_second)
    UseMethod("include_time_level")

#' @rdname include_time_level
#'
#' @export
include_time_level.timedimension <-
  function(td,
           include_time = TRUE,
           include_minute = TRUE,
           include_second = TRUE) {
    stopifnot("'include_time' must be of logical type." = is.logical(include_time))
    stopifnot("'include_minute' must be of logical type." = is.logical(include_minute))
    stopifnot("'include_second' must be of logical type." = is.logical(include_second))
    td$include_time <- include_time
    td$include_minute <- include_minute
    td$include_second <- include_second
    if (td$include_second) {
      td$include_minute <- TRUE
    }
    td
  }
