.onLoad <- function(libname, pkgname) {
  utils::data("time_seconds",
              "date_days",
              package = pkgname,
              envir = parent.env(environment()))
}


#' `when` S3 class
#'
#' Creates a `when` object.
#'
#' Using the parameters of this function we can configure practically  all the
#' elements of the dimension. Alternatively, we can use the configuration functions
#' to define the available options.
#'
#' We discuss the parameters in each of the specific configuration functions.
#'
#' @param name A string, table name.
#' @param type A string, type of calendar (NULL, 'iso', 'epi' or 'time').
#' @param locale A locale, to use for day and month names.
#' @param start A string, start of the period to be included in the dimension.
#' @param end A string, end of the period to be included in the dimension.
#' @param values A vector of string.
#' @param ... Rest of boolean configuration parameters.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#' @seealso \code{\link{generate_table}}, \code{\link{get_table}}
#'
#' @examples
#'
#' td_1 <- when()
#'
#' td_2 <- when(type = 'time')
#'
#' @export
when <- function(name = NULL,
                 type = NULL,
                 locale = Sys.getlocale("LC_TIME"),
                 start = lubridate::today(),
                 end = lubridate::today(),
                 values = NULL,
                 ...) {
  levels_t <- c("time")
  levels_d <- c("day", "week", "month", "year")
  levels <- c(levels_t, levels_d)

  year_y <- c("year")
  year_n <- c("decade")
  week_y <- c("year_week", "week")
  week_n <- c("week_date")
  month_y <- c("year_month",
               "month",
               "month_name",
               "month_num_name")
  month_n <- c(
    "month_abbr",
    "month_num_abbr",
    "year_quarter",
    "quarter",
    "year_semester",
    "semester"
  )
  day_y <- c(
    "date",
    "month_day",
    "week_day",
    "day_name",
    "day_num_name"
  )
  day_n <- c("day_abbr",
             "day_num_abbr",
             "year_day",
             "quarter_day")
  time_y <-
    c("time", "hour", "minute", "second", "day_part")
  time_n <- NULL

  att <- c(time_y,
           time_n,
           day_y,
           day_n,
           week_y,
           week_n,
           month_y,
           month_n,
           year_y,
           year_n)

  att_include <- rep(TRUE, length(att))
  names(att_include) <- att
  for (l in levels) {
    att_include[eval(parse(text = paste0(l, '_n')))] <- FALSE
  }

  att_levels <- rep("", length(att))
  names(att_levels) <- att
  for (l in levels) {
    att_levels[c(eval(parse(text = paste0(l, '_y'))), eval(parse(text = paste0(l, '_n'))))] <-
      l
  }

  level_include <- rep(TRUE, length(levels))
  names(level_include) <- levels
  level_include[levels_t] <- FALSE

  level_type <- rep("date", length(levels))
  names(level_type) <- levels
  level_type[levels_t] <- "time"

  surrogate_key <- TRUE
  week_starts_monday <- TRUE
  att_include_conf <- att_include
  level_include_conf <- level_include
  nl <- paste0(levels, '_level')
  att_o <- c('surrogate_key', 'week_starts_monday')
  dots <- list(...)
  for (n in names(dots)) {
    stopifnot("The additional parameters must be of logical type." = is.logical(dots[[n]]))
    stopifnot("There are additional parameters that are not considered." = n %in% c(att, att_o, nl))
    if (n %in% att_o) {
      assign(n, dots[[n]])
    } else if (n %in% nl) {
      nom <- gsub('_level', '', n)
      level_include_conf[nom] <- dots[[n]]
    } else {
      att_include_conf[n] <- dots[[n]]
    }
  }
  att_include_conf['hour'] <- TRUE
  if (!att_include_conf['minute']) {
    att_include_conf['second'] <- FALSE
  }

  if (!is.null(name)) {
    stopifnot("'name' must have a single value." = length(name) == 1)
  }

  day_part <-
    c(
      rep('Night', 5),
      rep('Morning', 7),
      rep('Afternoon', 5),
      rep('Evening', 4),
      rep('Night', 3)
    )
  names(day_part) <- sprintf("%02d", 0:23)

  td <- structure(
    list(
      type = type,
      locale = locale,
      start = start,
      end = end,
      values = values,
      surrogate_key = surrogate_key,
      week_starts_monday = week_starts_monday,

      att_levels = att_levels,
      level_type = level_type,
      att_include_conf = att_include_conf,
      level_include_conf = level_include_conf,

      day_part = day_part,
      table_name = name,
      attribute_names = NULL,
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
        start <- complete_times(start)
        end <- complete_times(end)
        start <- hms::as_hms(start)
        end <- hms::as_hms(end)
      } else {
        start <- hms::as_hms("00:00:00")
        end <- hms::as_hms("23:59:59")
      }
    } else {
      start <- complete_dates(start)
      end <- complete_dates(end)
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
      values <- complete_times(values)
      values <- hms::as_hms(values)
    } else {
      values <- complete_dates(values)
      values <- lubridate::ymd(values)
    }
    td$values <- sort(unique(values))
  }
  td
}


#' Complete time values
#'
#' @param values A vector of string.
#'
#' @return A vector of string.
#'
#' @keywords internal
complete_times <- function(values) {
  values <- as.character(values)
  len <- nchar(values)
  values[len == 1] <- sprintf("%02d", as.integer(values[len == 1]))
  len <- nchar(values)
  values[len == 2] <- paste0(values[len == 2], ":00:00")
  values[len == 5] <- paste0(values[len == 5], ":00")
  values
}


#' Complete date values
#'
#' @param values A vector of string.
#'
#' @return A vector of string.
#'
#' @keywords internal
complete_dates <- function(values) {
  values <- as.character(values)
  len <- nchar(values)
  values[len == 4] <- paste0(values[len == 4], "-01-01")
  values[len == 7] <- paste0(values[len == 7], "-01")
  values
}
