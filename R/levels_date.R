#' Configure date levels
#'
#' When the dimension is defined as date type, using this function we can select
#' the levels to include in it: day, week, month, quarter, semester and year.
#'
#' @param td A `when` object.
#' @param include_all A boolean, include all levels.
#' @param exclude_all A boolean, exclude all levels.
#' @param day_level A boolean, include day level.
#' @param week_level A boolean, include week level.
#' @param month_level A boolean, include month level.
#' @param quarter_level A boolean, include quarter level.
#' @param semester_level A boolean, include semester level.
#' @param year_level A boolean, include year level.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   select_date_levels(week_level = FALSE)
#'
#' @export
select_date_levels <-
  function(td,
           include_all,
           exclude_all,
           day_level,
           week_level,
           month_level,
           quarter_level,
           semester_level,
           year_level)
    UseMethod("select_date_levels")

#' @rdname select_date_levels
#'
#' @export
select_date_levels.when <-
  function(td,
           include_all = FALSE,
           exclude_all = FALSE,
           day_level = NULL,
           week_level = NULL,
           month_level = NULL,
           quarter_level = NULL,
           semester_level = NULL,
           year_level = NULL) {
    stopifnot("'include_all' must be of logical type." = is.logical(include_all))
    stopifnot("'exclude_all' must be of logical type." = is.logical(exclude_all))
    stopifnot(
      "Only one of the options can be selected: include or exclude all." = !(include_all &
                                                                               exclude_all)
    )
    date_levels <- names(td$level_type[td$level_type == 'date'])
    if (include_all) {
      td$level_include_conf[date_levels] <- TRUE
    } else if (exclude_all) {
      td$level_include_conf[date_levels] <- FALSE
    }
    for (l in date_levels) {
      v <- eval(parse(text = paste0(l, '_level')))
      if (!is.null(v)) {
        stopifnot("The parameters must be of logical type." = is.logical(v))
        td$level_include_conf[l] <- v
      }
    }
    td
  }


#' Configure date level (common)
#'
#' @param td A `when` object.
#' @param include_all A boolean, include all fields of the level.
#' @param exclude_all A boolean, exclude all fields of the level.
#' @param ... Rest of boolean configuration parameters.
#'
#' @return A `when` object.
#'
#' @keywords internal
select_date_level_common <- function(td,
                                     name = NULL,
                                     include_all = FALSE,
                                     exclude_all = FALSE,
                                     ...) {
  stopifnot("'name' must be a date level." = name %in% names(td$level_type[td$level_type == 'date']))
  stopifnot("'include_all' must be of logical type." = is.logical(include_all))
  stopifnot("'exclude_all' must be of logical type." = is.logical(exclude_all))
  stopifnot(
    "Only one of the options can be selected: include or exclude all." = !(include_all &
                                                                             exclude_all)
  )

  att <- names(td$att_levels[td$att_levels == name])
  if (include_all) {
    td$att_include_conf[att] <- TRUE
    td$level_include_conf[name] <- TRUE
  } else if (exclude_all) {
    td$att_include_conf[att] <- FALSE
    td$level_include_conf[name] <- FALSE
  }
  dots <- list(...)
  for (n in names(dots)) {
    if (!is.null(dots[[n]])) {
      stopifnot("The additional parameters must be of logical type." = is.logical(dots[[n]]))
      stopifnot("There are additional parameters that are not considered." = n %in% att)
      td$att_include_conf[n] <- dots[[n]]
      if (dots[[n]]) {
        td$level_include_conf[name] <- TRUE
      }
    }
  }
  td
}


#' Configure year level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the year level and its attributes to include in it: year and decade.
#'
#' The `include_all` and `exclude_all` parameters allow us to include or exclude
#' all attributes, and then specifically exclude or include the ones we need.
#'
#' @param td A `when` object.
#' @param include_all A boolean, include all fields of the level.
#' @param exclude_all A boolean, exclude all fields of the level.
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
#'   select_year_level(decade = FALSE)
#'
#' @export
select_year_level <-
  function(td,
           include_all,
           exclude_all,
           year,
           decade)
    UseMethod("select_year_level")

#' @rdname select_year_level
#'
#' @export
select_year_level.when <-
  function(td,
           include_all = FALSE,
           exclude_all = FALSE,
           year = NULL,
           decade = NULL) {
    select_date_level_common(
      td,
      name = 'year',
      include_all,
      exclude_all,
      year = year,
      decade = decade
    )
  }

#' Configure week level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the week level and its attributes to include in it: week and year_week.
#'
#' The `include_all` and `exclude_all` parameters allow us to include or exclude
#' all attributes, and then specifically exclude or include the ones we need.
#'
#' For the first and last days of the year, the year associated with the week may
#' be different from the year of the date, depending on the date type selected.
#'
#' The week number associated with each date depends on the type of date dimension
#' selected: standard ('date'), ISO 8601 ('iso') or epidemiological ('epi').
#'
#' The standard week numbers blocks of 7 days beginning on January 1. The last week
#' of the year can be less than 7 days long.
#'
#' The ISO 8601 week numbers blocks of 7 days from Monday to Sunday. The first and
#' last week of the year can contain days from the previous or next year.
#'
#' The epidemiological week is like ISO 8601 only that it considers that the week
#' begins on Sunday.
#'
#' @param td A `when` object.
#' @param include_all A boolean, include all fields of the level.
#' @param exclude_all A boolean, exclude all fields of the level.
#' @param week A boolean, include the week number.
#' @param year_week A boolean, include the year-week combination.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   select_week_level(year_week = FALSE)
#'
#' @export
select_week_level <-
  function(td,
           include_all,
           exclude_all,
           week,
           year_week)
    UseMethod("select_week_level")

#' @rdname select_week_level
#'
#' @export
select_week_level.when <-
  function(td,
           include_all = FALSE,
           exclude_all = FALSE,
           week = NULL,
           year_week = NULL) {
    select_date_level_common(
      td,
      name = 'week',
      include_all,
      exclude_all,
      week = week,
      year_week = year_week
    )
  }


#' Configure month level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the month level and its attributes to include in it. We can also obtain the
#' combination of the year with the month number.
#'
#' For the month we have the month number in the year, its name and the abbreviation
#' of the name. So that the order of the names corresponds to the alphabetical order,
#' the combination of month number and name and/or abbreviation is included.
#'
#' The `include_all` and `exclude_all` parameters allow us to include or exclude
#' all attributes, and then specifically exclude or include the ones we need.
#'
#' @param td A `when` object.
#' @param include_all A boolean, include all fields of the level.
#' @param exclude_all A boolean, exclude all fields of the level.
#' @param month A boolean, include the month number.
#' @param year_month A boolean, include the year-month combination.
#' @param month_name A boolean, include the month name.
#' @param month_num_name A boolean, include the month number and name.
#' @param month_abbr A boolean, include the month name abbreviated version.
#' @param month_num_abbr A boolean, include the month number and name
#' abbreviated version.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   select_month_level(month_abbr = FALSE,
#'                       month_num_abbr = FALSE)
#'
#' @export
select_month_level <-
  function(td,
           include_all,
           exclude_all,
           month,
           year_month,
           month_name,
           month_num_name,
           month_abbr,
           month_num_abbr)
    UseMethod("select_month_level")

#' @rdname select_month_level
#'
#' @export
select_month_level.when <-
  function(td,
           include_all = FALSE,
           exclude_all = FALSE,
           month = NULL,
           year_month = NULL,
           month_name = NULL,
           month_num_name = NULL,
           month_abbr = NULL,
           month_num_abbr = NULL) {
    select_date_level_common(
      td,
      name = 'month',
      include_all,
      exclude_all,
      month = month,
      year_month = year_month,
      month_name = month_name,
      month_num_name = month_num_name,
      month_abbr = month_abbr,
      month_num_abbr = month_num_abbr
    )
  }


#' Configure quarter level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the quarter level and its attributes to include in it: quarter number and the
#' combination of the year with it.
#'
#' The `include_all` and `exclude_all` parameters allow us to include or exclude
#' all attributes, and then specifically exclude or include the ones we need.
#'
#' @param td A `when` object.
#' @param include_all A boolean, include all fields of the level.
#' @param exclude_all A boolean, exclude all fields of the level.
#' @param quarter A boolean, include the quarter field.
#' @param year_quarter A boolean, include the quarter field.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   select_quarter_level(quarter = FALSE)
#'
#' @export
select_quarter_level <-
  function(td,
           include_all,
           exclude_all,
           quarter,
           year_quarter)
    UseMethod("select_quarter_level")

#' @rdname select_quarter_level
#'
#' @export
select_quarter_level.when <-
  function(td,
           include_all = FALSE,
           exclude_all = FALSE,
           quarter = NULL,
           year_quarter = NULL) {
    select_date_level_common(
      td,
      name = 'quarter',
      include_all,
      exclude_all,
      quarter = quarter,
      year_quarter = year_quarter
    )
  }


#' Configure semester level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the semester level and its attributes to include in it: semester number and the
#' combination of the year with it.
#'
#' The `include_all` and `exclude_all` parameters allow us to include or exclude
#' all attributes, and then specifically exclude or include the ones we need.
#'
#' @param td A `when` object.
#' @param include_all A boolean, include all fields of the level.
#' @param exclude_all A boolean, exclude all fields of the level.
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
#'   select_semester_level(semester = FALSE)
#'
#' @export
select_semester_level <-
  function(td,
           include_all,
           exclude_all,
           semester,
           year_semester)
    UseMethod("select_semester_level")

#' @rdname select_semester_level
#'
#' @export
select_semester_level.when <-
  function(td,
           include_all = FALSE,
           exclude_all = FALSE,
           semester = NULL,
           year_semester = NULL) {
    select_date_level_common(
      td,
      name = 'semester',
      include_all,
      exclude_all,
      semester = semester,
      year_semester = year_semester
    )
  }

#' Configure day level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the day level and its attributes to include in it: date, month_day, week_day,
#' quarter_day and year_day.
#'
#' The `include_all` and `exclude_all` parameters allow us to include or exclude
#' all attributes, and then specifically exclude or include the ones we need.
#'
#' For the week_day we have the day number, its name and the name abbreviation.
#' So that the order of the names corresponds to the alphabetical order, the
#' combination of day number and name and/or abbreviation is included.
#'
#' @param td A `when` object.
#' @param include_all A boolean, include all fields of the level.
#' @param exclude_all A boolean, exclude all fields of the level.
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
#'   select_day_level(day_abbr = FALSE,
#'                     day_num_abbr = FALSE)
#'
#' @export
select_day_level <- function(td,
                             include_all,
                             exclude_all,
                             date,
                             month_day,
                             week_day,
                             day_name,
                             day_num_name,
                             day_abbr,
                             day_num_abbr,
                             quarter_day,
                             year_day)
UseMethod("select_day_level")

#' @rdname select_day_level
#'
#' @export
select_day_level.when <- function(td,
                                  include_all = FALSE,
                                  exclude_all = FALSE,
                                  date = NULL,
                                  month_day = NULL,
                                  week_day = NULL,
                                  day_name = NULL,
                                  day_num_name = NULL,
                                  day_abbr = NULL,
                                  day_num_abbr = NULL,
                                  quarter_day = NULL,
                                  year_day = NULL) {
  select_date_level_common(
    td,
    name = 'day',
    include_all,
    exclude_all,
    date = date,
    month_day = month_day,
    week_day = week_day,
    day_name = day_name,
    day_num_name = day_num_name,
    day_abbr = day_abbr,
    day_num_abbr = day_num_abbr,
    quarter_day = quarter_day,
    year_day = year_day
  )
}
