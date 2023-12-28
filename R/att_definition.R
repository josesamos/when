
#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_date <- function(data, values, ...) {
  data[['date']] <- values
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_year_day <- function(data, values, ...) {
  data[['year_day']] <- sprintf("%03d", lubridate::yday(values))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_quarter_day <- function(data, values, ...) {
  data[['quarter_day']] <- sprintf("%02d", lubridate::qday(values))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_month_day <- function(data, values, ...) {
  data[['month_day']] <- sprintf("%02d", lubridate::mday(values))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_week_day <- function(data, values, ...) {
  dots <- list(...)
  if (dots[['week_starts_monday']]) {
    v <- 1
  } else {
    v <- 7
  }
  data[['week_day']] <-
    as.character(lubridate::wday(values,
                                 week_start = getOption("lubridate.week.start", v)))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_day_name <- function(data, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  data[['day_name']] <- as.character(lubridate::wday(
    values,
    label = TRUE,
    abbr = FALSE,
    locale = locale
  ))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_day_num_name <- function(data, values, ...) {
  dots <- list(...)
  if (dots[['week_starts_monday']]) {
    v <- 1
  } else {
    v <- 7
  }
  locale <- dots[['locale']]
  data[['day_num_name']] <- paste0(
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
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_day_abbr <- function(data, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  data[['day_abbr']] <- gsub("\\\\.", "", as.character(
    lubridate::wday(
      values,
      label = TRUE,
      abbr = TRUE,
      locale = locale
    )
  ))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_day_num_abbr <- function(data, values, ...) {
  dots <- list(...)
  if (dots[['week_starts_monday']]) {
    v <- 1
  } else {
    v <- 7
  }
  locale <- dots[['locale']]
  data[['day_num_abbr']] <- paste0(
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
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_year_week <- function(data, values, ...) {
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
  data[['year_week']] <- paste0(year, '-', sprintf("%02d", week))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_week <- function(data, values, ...) {
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
  data[['week']] <- sprintf("%02d", week)
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_week_date <- function(data, values, ...) {
  data[['week_date']] <- values
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_year_semester <- function(data, values, ...) {
  data[['year_semester']] <- paste0(lubridate::year(values),
                                    '-',
                                    lubridate::semester(values))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_semester <- function(data, values, ...) {
  data[['semester']] <- as.character(lubridate::semester(values))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_year_quarter <- function(data, values, ...) {
  data[['year_quarter']] <- paste0(lubridate::year(values),
                                   '-',
                                   lubridate::quarter(values))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_quarter <- function(data, values, ...) {
  data[['quarter']] <- as.character(lubridate::quarter(values))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_year_month <- function(data, values, ...) {
  data[['year_month']] <- paste0(lubridate::year(values),
                                 '-',
                                 sprintf("%02d", lubridate::month(values)))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_month <- function(data, values, ...) {
  data[['month']] <- sprintf("%02d", lubridate::month(values))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_month_name <- function(data, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  data[['month_name']] <- as.character(lubridate::month(
    values,
    label = TRUE,
    abbr = FALSE,
    locale = locale
  ))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_month_num_name <- function(data, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  data[['month_num_name']] <- paste0(
    sprintf("%02d", lubridate::month(values)),
    '-',
    lubridate::month(
      values,
      label = TRUE,
      abbr = FALSE,
      locale = locale
    )
  )
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_month_abbr <- function(data, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  data[['month_abbr']] <- as.character(lubridate::month(
    values,
    label = TRUE,
    abbr = TRUE,
    locale = locale
  ))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_month_num_abbr <- function(data, values, ...) {
  dots <- list(...)
  locale <- dots[['locale']]
  data[['month_num_abbr']] <- paste0(
    sprintf("%02d", lubridate::month(values)),
    '-',
    lubridate::month(
      values,
      label = TRUE,
      abbr = TRUE,
      locale = locale
    )
  )
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_year <- function(data, values, ...) {
  data[['year']] <- as.character(lubridate::year(values))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_decade <- function(data, values, ...) {
  data[['decade']] <- as.character(10 * (lubridate::year(values) %/% 10))
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_time <- function(data, values, ...) {
  data[['time']] <- values
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_hour <- function(data, values, ...) {
  data[['hour']] <- substr(values, 1, 2)
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_minute <- function(data, values, ...) {
  data[['minute']] <- substr(values, 4, 5)
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_second <- function(data, values, ...) {
  data[['second']] <- substr(values, 7, 8)
  data
}

#' Get the data from values and fields
#'
#' @param data A `tibble`.
#' @param values A vector of strings.
#' @param ... Rest of configuration parameters.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data_day_part <- function(data, values, ...) {
  dots <- list(...)
  day_part <- dots[['day_part']]
  data[['day_part']] <- day_part[substr(values, 1, 2)]
  data
}


