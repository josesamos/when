#' Include time level
#'
#' When the dimension is defined as a time type, using this function we can select
#' its attributes to include in it: time, minute, second and day_part.
#'
#' The 'hour' attribute will always be included. If the 'minute' attribute is not
#' included the 'second' attribute will not be included either.
#'
#' @param td A `when` object.
#' @param include_time A boolean, include a field for the time.
#' @param include_minute A boolean, include the minute level of detail.
#' @param include_second A boolean, include the second level of detail.
#' @param include_day_part A boolean, include the parts of the day.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_time_level(include_day_part = FALSE)
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
    if (!td$include_minute) {
      td$include_second <- FALSE
    }
    td$include_day_part <- include_day_part
    td
  }


#' Set day part
#'
#' Using this function we can change the name assigned to the hours of the day to
#' designate the parts of the day.
#'
#' @param td A `when` object.
#' @param hour A number, hour number (between 0 and 23).
#' @param name a string, name of the part of the day.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   set_day_part(hour = c(21:23, 0:4), name = "Night")
#'
#' @export
set_day_part <-
  function(td,
           hour,
           name)
    UseMethod("set_day_part")

#' @rdname set_day_part
#'
#' @export
set_day_part.when <-
  function(td,
           hour = NULL,
           name = NULL) {
    stopifnot("'hour' is not defined." = !is.null(hour))
    stopifnot("'hour' is out of bounds." = all(0 <= hour & hour <= 23))
    stopifnot("'name' must have a single value." = length(name) == 1)

    hour <- sprintf("%02d", hour)
    td$day_part[hour]<- name
    td
  }


#' Get day part
#'
#' Get day part.
#'
#' @param td A `when` object.
#'
#' @return A named vector.
#'
#' @family dimension definition
#'
#' @examples
#'
#' dp <- when() |>
#'   get_day_part()
#'
#' @export
get_day_part <-
  function(td)
    UseMethod("get_day_part")

#' @rdname get_day_part
#'
#' @export
get_day_part.when <-
  function(td) {
    td$day_part
  }
