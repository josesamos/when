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
           include_day_part = TRUE) {
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


#' Define day part
#'
#' Define day part.
#'
#' @param td A `when` object.
#' @param hour A number.
#' @param name a string.
#'
#' @return A `when` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- when() |>
#'   define_day_part(hour = c(21:23, 0:4), name = "Night")
#'
#' @export
define_day_part <-
  function(td,
           hour,
           name)
    UseMethod("define_day_part")

#' @rdname define_day_part
#'
#' @export
define_day_part.when <-
  function(td,
           hour = NULL,
           name = NULL) {
    stopifnot("'hour' is not defined." = !is.null(hour))
    stopifnot("'hour' is out of bounds." = all(0 <= hour & hour <= 23))
    stopifnot("'name' must have a single value." = length(name) == 1)

    day_part <- sprintf("%02d", hour)
    names <- names(td$day_part)
    names(names) <- td$day_part
    names[day_part] <- name
    names(td$day_part) <- names
    td
  }
