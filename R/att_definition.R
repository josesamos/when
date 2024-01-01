#' Get attribute definition function
#'
#' Each attribute is defined by a function that adds a column to a table based on
#' the parameter that contains the date or time. This function returns the definition
#' function for the attribute whose name is given.
#'
#' @param td A `when` object.
#' @param name A string, attribute name.
#'
#' @return A function.
#'
#' @family dimension definition
#'
#' @examples
#'
#' f <- when() |>
#'   get_attribute_definition_function(name = "year")
#'
#' @export
get_attribute_definition_function <-
  function(td, name)
    UseMethod("get_attribute_definition_function")

#' @rdname get_attribute_definition_function
#'
#' @export
get_attribute_definition_function.when <-
  function(td, name = NULL) {
    stopifnot("The name of an attribute must be indicated." = !is.null(name))
    stopifnot("The name is not that of one of the defined attributes." = name %in% names(td$att_function))
    td$att_function[[name]]
  }


#' Set attribute definition function
#'
#' Each attribute is defined by a function that adds a column to a table based on
#' the parameter that contains the date or time. This function sets the definition
#' function for the attribute whose name is given.
#'
#' @param td A `when` object.
#' @param name A string, attribute name.
#' @param f A function.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' f <- function(table, values, ...) {
#'   table[['year']] <- 'Not defined'
#'   table
#' }
#'
#' wd <- when() |>
#'   set_attribute_definition_function(name = "year", f)
#'
#' @export
set_attribute_definition_function <-
  function(td, name, f)
    UseMethod("set_attribute_definition_function")

#' @rdname set_attribute_definition_function
#'
#' @export
set_attribute_definition_function.when <-
  function(td, name = NULL, f = NULL) {
    stopifnot("The name of an attribute must be indicated." = !is.null(name))
    stopifnot("The name is not that of one of the defined attributes." = name %in% names(td$att_function))
    td$att_function[[name]] <- f
    td
  }



#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_date <- function(table, values, ...) {
  table[['date']] <- values
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_year_day <- function(table, values, ...) {
  table[['year_day']] <- sprintf("%03d", lubridate::yday(values))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_quarter_day <- function(table, values, ...) {
  table[['quarter_day']] <- sprintf("%02d", lubridate::qday(values))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_month_day <- function(table, values, ...) {
  table[['month_day']] <- sprintf("%02d", lubridate::mday(values))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_week_day <- function(table, values, ...) {
  dots <- list(...)
  if (dots[['week_starts_monday']]) {
    v <- 1
  } else {
    v <- 7
  }
  table[['week_day']] <-
    as.character(lubridate::wday(values,
                                 week_start = getOption("lubridate.week.start", v)))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_day_name <- function(table, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  table[['day_name']] <- as.character(lubridate::wday(
    values,
    label = TRUE,
    abbr = FALSE,
    locale = locale
  ))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_day_num_name <- function(table, values, ...) {
  dots <- list(...)
  if (dots[['week_starts_monday']]) {
    v <- 1
  } else {
    v <- 7
  }
  locale <- dots[['locale']]
  table[['day_num_name']] <- paste0(
    lubridate::wday(values,
                    week_start = getOption("lubridate.week.start", v)),
    '-',
    lubridate::wday(
      values,
      label = TRUE,
      abbr = FALSE,
      locale = locale
    )
  )
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_day_abbr <- function(table, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  table[['day_abbr']] <- gsub("\\\\.", "", as.character(
    lubridate::wday(
      values,
      label = TRUE,
      abbr = TRUE,
      locale = locale
    )
  ))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_day_num_abbr <- function(table, values, ...) {
  dots <- list(...)
  if (dots[['week_starts_monday']]) {
    v <- 1
  } else {
    v <- 7
  }
  locale <- dots[['locale']]
  table[['day_num_abbr']] <- paste0(
    lubridate::wday(values,
                    week_start = getOption("lubridate.week.start", v)),
    '-',
    gsub("\\\\.", "", as.character(
      lubridate::wday(
        values,
        label = TRUE,
        abbr = TRUE,
        locale = locale
      )
    ))
  )
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_year_week <- function(table, values, ...) {
  dots <- list(...)
  type <- dots[['type']]
  switch(type,
         iso = {
           year <- lubridate::isoyear(values)
           week <- lubridate::isoweek(values)
         },
         epi = {
           year <- lubridate::epiyear(values)
           week <- lubridate::epiweek(values)
         },
         {
           year <- lubridate::year(values)
           week <- lubridate::week(values)
         })
  table[['year_week']] <- paste0(year, '-', sprintf("%02d", week))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_week <- function(table, values, ...) {
  dots <- list(...)
  type <- dots[['type']]
  switch(type,
         iso = {
           week <- lubridate::isoweek(values)
         },
         epi = {
           week <- lubridate::epiweek(values)
         },
         {
           week <- lubridate::week(values)
         })
  table[['week']] <- sprintf("%02d", week)
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_year_semester <- function(table, values, ...) {
  table[['year_semester']] <- paste0(lubridate::year(values),
                                    '-',
                                    lubridate::semester(values))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_semester <- function(table, values, ...) {
  table[['semester']] <- as.character(lubridate::semester(values))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_year_quarter <- function(table, values, ...) {
  table[['year_quarter']] <- paste0(lubridate::year(values),
                                   '-',
                                   lubridate::quarter(values))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_quarter <- function(table, values, ...) {
  table[['quarter']] <- as.character(lubridate::quarter(values))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_year_month <- function(table, values, ...) {
  table[['year_month']] <- paste0(lubridate::year(values),
                                 '-',
                                 sprintf("%02d", lubridate::month(values)))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_month <- function(table, values, ...) {
  table[['month']] <- sprintf("%02d", lubridate::month(values))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_month_name <- function(table, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  table[['month_name']] <- as.character(lubridate::month(
    values,
    label = TRUE,
    abbr = FALSE,
    locale = locale
  ))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_month_num_name <- function(table, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  table[['month_num_name']] <- paste0(
    sprintf("%02d", lubridate::month(values)),
    '-',
    lubridate::month(
      values,
      label = TRUE,
      abbr = FALSE,
      locale = locale
    )
  )
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_month_abbr <- function(table, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  table[['month_abbr']] <- as.character(lubridate::month(
    values,
    label = TRUE,
    abbr = TRUE,
    locale = locale
  ))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_month_num_abbr <- function(table, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  table[['month_num_abbr']] <- paste0(
    sprintf("%02d", lubridate::month(values)),
    '-',
    lubridate::month(
      values,
      label = TRUE,
      abbr = TRUE,
      locale = locale
    )
  )
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_year <- function(table, values, ...) {
  table[['year']] <- as.character(lubridate::year(values))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_decade <- function(table, values, ...) {
  table[['decade']] <- as.character(10 * (lubridate::year(values) %/% 10))
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_time <- function(table, values, ...) {
  table[['time']] <- values
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_hour <- function(table, values, ...) {
  table[['hour']] <- substr(values, 1, 2)
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_minute <- function(table, values, ...) {
  table[['minute']] <- substr(values, 4, 5)
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_second <- function(table, values, ...) {
  table[['second']] <- substr(values, 7, 8)
  table
}

#' Get the table from values and fields
#'
#' @param table A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_table_day_part <- function(table, values, ...) {
  dots <- list(...)
  day_part <- dots[['day_part']]
  table[['day_part']] <- day_part[substr(values, 1, 2)]
  table
}
