.onLoad <- function(libname, pkgname) {
  utils::data("time_seconds",
              package = pkgname,
              envir = parent.env(environment()))
}

#' `when` S3 class
#'
#' Creates a `when` object.
#'
#' @param type A string, type of calendar (NULL, 'iso', 'epi' or 'time').
#' @param locale A locale, to use for day and month names.
#' @param start A string, start of the period to be included in the dimension.
#' @param end A string, end of the period to be included in the dimension.
#' @param values A vector of string.
#' @param ... Rest of boolean configuration parameters.
#'
#' @return A `when` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- when()
#'
#' @export
when <- function(type = NULL,
                 locale = Sys.getlocale("LC_TIME"),
                 start = lubridate::today(),
                 end = lubridate::today(),
                 values = NULL,
                 ...) {
  surrogate_key <- TRUE
  week_starts_monday <- TRUE
  day_level <- TRUE
  week_level <- TRUE
  month_level <- TRUE
  year_level <- TRUE
  hour_level <- FALSE
  include_year <- TRUE
  include_decade <- FALSE
  include_week <- TRUE
  include_year_week <- TRUE
  include_month <- TRUE
  include_year_month <- TRUE
  include_month_name <- TRUE
  include_month_abbr <- FALSE
  include_month_num_name <- TRUE
  include_month_num_abbr <- FALSE
  include_quarter <- FALSE
  include_year_quarter <- FALSE
  include_semester <- FALSE
  include_year_semester <- FALSE
  include_month_day <- TRUE
  include_week_day <- TRUE
  include_day_name <- TRUE
  include_day_abbr <- FALSE
  include_day_num_name <- TRUE
  include_day_num_abbr <- FALSE
  include_quarter_day <- FALSE
  include_year_day <- FALSE
  include_date <- TRUE
  include_time <- TRUE
  include_minute <- TRUE
  include_second <- TRUE
  dots <- list(...)
  for (n in names(dots)) {
    stopifnot("The additional parameters must be of logical type." = is.logical(dots[[n]]))
    assign(n, dots[[n]])
  }
  include_hour <- TRUE

  td <- structure(
    list(
      type = type,
      locale = locale,
      start = start,
      end = end,
      values = values,
      surrogate_key = surrogate_key,
      week_starts_monday = week_starts_monday,
      levels = c("hour", "day", "week", "month", "year"),

      year_level = year_level,
      year_level_names = c("year", "decade"),
      include_year = include_year,
      include_decade = include_decade,

      week_level = week_level,
      week_level_names = c("year_week", "week"),
      include_week = include_week,
      include_year_week = include_year_week,

      month_level = month_level,
      month_level_names = c(
        "year_semester",
        "semester",
        "year_quarter",
        "quarter",
        "year_month",
        "month",
        "month_name",
        "month_num_name",
        "month_abbr",
        "month_num_abbr"
      ),
      include_month = include_month,
      include_year_month = include_year_month,
      include_month_name = include_month_name,
      include_month_abbr = include_month_abbr,
      include_month_num_name = include_month_num_name,
      include_month_num_abbr = include_month_num_abbr,
      include_quarter = include_quarter,
      include_year_quarter = include_year_quarter,
      include_semester = include_semester,
      include_year_semester = include_year_semester,

      day_level = day_level,
      day_level_names = c(
        "date",
        "year_day",
        "quarter_day",
        "month_day",
        "week_day",
        "day_name",
        "day_num_name",
        "day_abbr",
        "day_num_abbr"
      ),
      include_month_day = include_month_day,
      include_week_day = include_week_day,
      include_day_name = include_day_name,
      include_day_abbr = include_day_abbr,
      include_day_num_name = include_day_num_name,
      include_day_num_abbr = include_day_num_abbr,
      include_quarter_day = include_quarter_day,
      include_year_day = include_year_day,
      include_date = include_date,

      hour_level = hour_level,
      hour_level_names = c("time", "hour", "minute", "second"),
      include_time = include_time,
      include_hour = include_hour,
      include_minute = include_minute,
      include_second = include_second,
      table = NULL
    ),
    class = "when"
  )
  td <- validate_type(td, type)
  td <- validate_start_end(td, start, end)
  td <- validate_values(td, values)
  td
}


#' Validate start and end parameters
#'
#' @param td A `when` object.
#' @param start A string, start of the period to be included in the dimension.
#' @param end A string, end of the period to be included in the dimension.
#'
#' @return A `when` object.
#'
#' @keywords internal
validate_start_end <- function(td, start, end) {
  if ((!is.null(start)) & (!is.null(end))) {
    if (td$type == 'time') {
      if (start != end) {
        start <- hms::as_hms(start)
        end <- hms::as_hms(end)
      } else {
        start <- hms::as_hms("00:00:00")
        end <- hms::as_hms("23:59:59")
      }
    } else {
      start <- lubridate::ymd(start)
      end <- lubridate::ymd(end)
    }
    stopifnot("The beginning of the period must be before the end of it." = start <= end)
    td$start <- start
    td$end <- end
  }
  td
}


#' Validate values parameter
#'
#' @param td A `when` object.
#' @param values A vector of string.
#'
#' @return A `when` object.
#'
#' @keywords internal
validate_values <- function(td, values) {
  if (!is.null(values)) {
    if (td$type == 'time') {
      values <- hms::as_hms(values)
    } else {
      values <- lubridate::ymd(values)
    }
    td$values <- sort(unique(values))
  }
  td
}
