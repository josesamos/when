#' Configure time level
#'
#' When the dimension is defined as a time type, using this function we can select
#' its attributes to include in it: time, minute, second and day_part.
#'
#' The 'hour' attribute will always be included. If the 'minute' attribute is not
#' included the 'second' attribute will not be included either.
#'
#' @param td A `when` object.
#' @param include_all A boolean, include all fields of the level.
#' @param exclude_all A boolean, exclude all fields of the level.
#' @param time A boolean, include a field for the time.
#' @param minute A boolean, include the minute level of detail.
#' @param second A boolean, include the second level of detail.
#' @param day_part A boolean, include the parts of the day.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   configure_time_level(day_part = FALSE)
#'
#' @export
configure_time_level <-
  function(td,
           include_all,
           exclude_all,
           time,
           minute,
           second,
           day_part)
    UseMethod("configure_time_level")

#' @rdname configure_time_level
#'
#' @export
configure_time_level.when <-
  function(td,
           include_all = FALSE,
           exclude_all = FALSE,
           time = NULL,
           minute = NULL,
           second = NULL,
           day_part = NULL) {
    stopifnot("'include_all' must be of logical type." = is.logical(include_all))
    stopifnot("'exclude_all' must be of logical type." = is.logical(exclude_all))
    stopifnot("Only one of the options can be selected: include or exclude all." = !(include_all & exclude_all))
    att <- names(td$att_levels[td$att_levels == 'time'])
    att <- setdiff(att, 'hour')
    if (include_all) {
      td$att_include_conf[att] <- TRUE
    } else if (exclude_all) {
      td$att_include_conf[att] <- FALSE
    }
    for (n in att) {
      v <- eval(parse(text = n))
      if (!is.null(v)) {
        stopifnot("The parameters must be of logical type." = is.logical(v))
        td$att_include_conf[n] <- v
      }
    }
    if (!td$att_include_conf['minute']) {
      td$att_include_conf['second'] <- FALSE
    }
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
