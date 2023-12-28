#' Include date levels
#'
#' When the dimension is defined as date type, using this function we can select
#' the levels to include in it: day, week, month and year.
#'
#' @param td A `when` object.
#' @param day_level A boolean, include day level.
#' @param week_level A boolean, include week level.
#' @param month_level A boolean, include month level.
#' @param year_level A boolean, include year level.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_date_levels(week_level = FALSE)
#'
#' @export
include_date_levels <-
  function(td,
           day_level,
           week_level,
           month_level,
           year_level)
    UseMethod("include_date_levels")

#' @rdname include_date_levels
#'
#' @export
include_date_levels.when <-
  function(td,
           day_level = TRUE,
           week_level = TRUE,
           month_level = TRUE,
           year_level = TRUE) {
    for (l in names(td$level_type[td$level_type == 'date'])) {
      v <- eval(parse(text = paste0(l, '_level')))
      stopifnot("The parameters must be of logical type." = is.logical(v))
      td$level_include_conf[l] <- v
    }
    td
  }


#' Include date level (common)
#'
#' @param td A `when` object.
#' @param level A boolean, include year level.
#' @param include_all A boolean, include all fields of the level.
#' @param ... Rest of boolean configuration parameters.
#'
#' @return A `when` object.
#'
#' @keywords internal
include_date_level_common <- function(td,
                                      name = NULL,
                                      level = TRUE,
                                      include_all = FALSE,
                                      ...) {
  stopifnot("'name' must be a date level." = name %in% names(td$level_type[td$level_type == 'date']))
  stopifnot("'level' must be of logical type." = is.logical(level))
  stopifnot("'include_all' must be of logical type." = is.logical(include_all))

  td$level_include_conf[name] <- level
  att <- names(td$att_levels[td$att_levels == name])
  if (include_all) {
    td$att_include_conf[att] <- TRUE
  } else {
    dots <- list(...)
    for (n in names(dots)) {
      stopifnot("The additional parameters must be of logical type." = is.logical(dots[[n]]))
      stopifnot("There are additional parameters that are not considered." = n %in% att)
      td$att_include_conf[n] <- dots[[n]]
    }
  }
  td
}


#' Include year level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the year level and its attributes to include in it: year and decade.
#'
#' @param td A `when` object.
#' @param level A boolean, include year level.
#' @param include_all A boolean, include all fields of the level.
#' @param year A boolean, include the year field.
#' @param decade A boolean, include the decade field.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_year_level(decade = FALSE)
#'
#' @export
include_year_level <-
  function(td,
           level,
           include_all,
           year,
           decade)
    UseMethod("include_year_level")

#' @rdname include_year_level
#'
#' @export
include_year_level.when <-
  function(td,
           level = TRUE,
           include_all = FALSE,
           year = TRUE,
           decade = FALSE) {
    include_date_level_common(td,
                              name = 'year',
                              level,
                              include_all,
                              year = year,
                              decade = decade)
  }

#' Include week level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the week level and its attributes to include in it: week, year_week and week_date.
#'
#' For the first and last days of the year, the year associated with the week may
#' be different from the year of the date, depending on the date type selected.
#'
#' The week number associated with each date depends on the type of date dimension
#' selected: standard (NULL), ISO 8601 ('iso') or epidemiological ('epi').
#'
#' The standard week numbers blocks of 7 days beginning on January 1. The last week
#' of the year can be less than 7 days long.
#'
#' The ISO 8601 week numbers blocks of 7 days from Monday to Sunday. The first and
#' last week of the year can contain days from the previous or next year.
#'
#' The epidemiological week is like ISO 8601 only that it considers that the week
#' begins on Sunday, it numbers blocks from Sunday to Monday.
#'
#' @param td A `when` object.
#' @param level A boolean, include week level.
#' @param include_all A boolean, include all fields of the level.
#' @param week A boolean, include the week number.
#' @param year_week A boolean, include the year-week combination.
#' @param week_date A boolean, include the date from which the week is
#' obtained.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_week_level(week_date = FALSE)
#'
#' @export
include_week_level <-
  function(td,
           level,
           include_all,
           week,
           year_week,
           week_date)
    UseMethod("include_week_level")

#' @rdname include_week_level
#'
#' @export
include_week_level.when <-
  function(td,
           level = TRUE,
           include_all = FALSE,
           week = TRUE,
           year_week = TRUE,
           week_date = FALSE) {
    include_date_level_common(td,
                              name = 'week',
                              level,
                              include_all,
                              week = week,
                              year_week = year_week,
                              week_date = week_date)
  }


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
#' @param month_num_name A boolean, include the month number and name.
#' @param month_abbr A boolean, include the month name abbreviated version.
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
           month_num_name,
           month_abbr,
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
           month_num_name = TRUE,
           month_abbr = FALSE,
           month_num_abbr = FALSE,
           quarter = FALSE,
           year_quarter = FALSE,
           semester = FALSE,
           year_semester = FALSE) {
    include_date_level_common(td,
                              name = 'month',
                              level,
                              include_all,
                              month = month,
                              year_month = year_month,
                              month_name = month_name,
                              month_num_name = month_num_name,
                              month_abbr = month_abbr,
                              month_num_abbr = month_num_abbr,
                              quarter = quarter,
                              year_quarter = year_quarter,
                              semester = semester,
                              year_semester = year_semester)
  }

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
#' @param level A boolean, include day level.
#' @param include_all A boolean, include all fields of the level.
#' @param date A boolean, include the date.
#' @param month_day A boolean, include the day number in the month.
#' @param week_day A boolean, the day number in the week.
#' @param day_name A boolean, include the name of the day of the week.
#' @param day_num_name A boolean, include the number and name of the day
#' of the week.
#' @param day_abbr A boolean, include the name of the day of the week in
#' abbreviated version.
#' @param day_num_abbr A boolean, include the number and name of the day
#' of the week in abbreviated version.
#' @param quarter_day A boolean, include the number of the day in the quarter.
#' @param year_day A boolean, include the number of the day in the year.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_day_level(day_abbr = FALSE,
#'                     day_num_abbr = FALSE)
#'
#' @export
include_day_level <- function(td,
                              level,
                              include_all,
                              date,
                              month_day,
                              week_day,
                              day_name,
                              day_num_name,
                              day_abbr,
                              day_num_abbr,
                              quarter_day,
                              year_day)
UseMethod("include_day_level")

#' @rdname include_day_level
#'
#' @export
include_day_level.when <- function(td,
                                   level = TRUE,
                                   include_all = FALSE,
                                   date = TRUE,
                                   month_day = TRUE,
                                   week_day = TRUE,
                                   day_name = TRUE,
                                   day_num_name = TRUE,
                                   day_abbr = FALSE,
                                   day_num_abbr = FALSE,
                                   quarter_day = FALSE,
                                   year_day = FALSE) {
  include_date_level_common(td,
                            name = 'day',
                            level,
                            include_all,
                            date = date,
                            month_day = month_day,
                            week_day = week_day,
                            day_name = day_name,
                            day_num_name = day_num_name,
                            day_abbr = day_abbr,
                            day_num_abbr = day_num_abbr,
                            quarter_day = quarter_day,
                            year_day = year_day)
}
