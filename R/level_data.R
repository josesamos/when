#' Get the table of the dimension
#'
#' Obtain the table of the defined dimension.
#'
#' @param td A `when` object.
#'
#' @return A `tibble`, the table.
#'
#' @family obtaining results
#'
#' @examples
#'
#' table <- when() |>
#'   get_level_data()
#'
#' @importFrom rlang :=
#'
#' @export
get_level_data <-
  function(td)
    UseMethod("get_level_data")

#' @rdname get_level_data
#'
#' @export
get_level_data.when <-
  function(td) {
    fields <- get_fields(td)
    values <- get_values(td)
    data <- get_data(td, values, fields)
    if (td$surrogate_key) {
      surrogate_key <- "id"
      data <-
        tibble::add_column(data, !!surrogate_key := 1:nrow(data), .before = 1)
    }
    data
  }


#' Get the fields according to the defined configuration
#'
#' @param td A `when` object.
#'
#' @return A vector of string.
#'
#' @keywords internal
get_fields <- function(td) {
  fields <- NULL
  for (level in td$levels) {
    if (td[[paste0(level, '_level')]]) {
      for (name in td[[paste0(level, '_level_names')]]) {
        if (td[[paste0('include_', name)]]) {
          fields <- c(fields, name)
        }
      }
    }
  }
  if (!('minute' %in% fields)) {
    fields <- setdiff(fields, "second")
  }
  fields
}

#' Get the values according to the defined configuration
#'
#' @param td A `when` object.
#'
#' @return A vector of string.
#'
#' @keywords internal
get_values <- function(td) {
  if (!is.null(td$values)) {
    values <- td$values
  } else {
    values <- NULL
    val <- td$start
    end <- td$end
    if (td$type == 'time') {
      if (val == end) {
        val <- hms::as_hms("00:00:00")
        if (td$include_minute) {
          if (td$include_second) {
            end <- hms::as_hms("23:59:59")
            td$start <- "00:00:00"
            td$end <- "23:59:59"
          } else {
            end <- hms::as_hms("23:59:00")
          }
        } else {
          end <- hms::as_hms("23:00:00")
        }
      }
      if (td$include_minute) {
        if (td$include_second) {
          inc <- 1
        } else {
          inc <- 60
        }
      } else {
        inc <- 3600
      }
      if (inc == 1) {
        values <-
          time_seconds[time_seconds >= td$start & time_seconds <= td$end]
      } else {
        while (val < end) {
          values <- c(values, as.character(val))
          val <- hms::as_hms(val + inc)
        }
        values <- c(values, as.character(end))
      }
    } else {
      while (val <= end) {
        values <- c(values, as.character(val))
        val <- lubridate::ymd(val) + 1
      }
    }
  }
  values
}


#' Get the data from values and fields
#'
#' @param td A `when` object.
#' @param values A vector of strings.
#' @param fields A vector of strings.
#'
#' @return A `tibble`.
#'
#' @keywords internal
get_data <- function(td, values, fields) {
  data <- list()
  for (f in fields) {
    switch(
      f,
      date = {
        data[[f]] <- values
      },
      year_day = {
        data[[f]] <- sprintf("%03d", lubridate::yday(values))
      },
      quarter_day = {
        data[[f]] <- sprintf("%02d", lubridate::qday(values))
      },
      month_day = {
        data[[f]] <- sprintf("%02d", lubridate::mday(values))
      },
      week_day = {
        if (td$week_starts_monday) {
          v <- 1
        } else {
          v <- 7
        }
        data[[f]] <-
          as.character(lubridate::wday(values,
                                       week_start = getOption("lubridate.week.start", v)))
      },
      day_name = {
        data[[f]] <- as.character(lubridate::wday(
          values,
          label = TRUE,
          abbr = FALSE,
          locale = td$locale
        ))
      },
      day_num_name = {
        if (td$week_starts_monday) {
          v <- 1
        } else {
          v <- 7
        }
        data[[f]] <- paste0(
          lubridate::wday(values,
                          week_start = getOption("lubridate.week.start", v)),
          '-',
          lubridate::wday(
            values,
            label = TRUE,
            abbr = FALSE,
            locale = td$locale
          )
        )
      },
      day_abbr = {
        data[[f]] <- gsub("\\\\.", "", as.character(
          lubridate::wday(
            values,
            label = TRUE,
            abbr = TRUE,
            locale = td$locale
          )
        ))
      },
      day_num_abbr = {
        if (td$week_starts_monday) {
          v <- 1
        } else {
          v <- 7
        }
        data[[f]] <- paste0(
          lubridate::wday(values,
                          week_start = getOption("lubridate.week.start", v)),
          '-',
          gsub("\\\\.", "", as.character(
            lubridate::wday(
              values,
              label = TRUE,
              abbr = TRUE,
              locale = td$locale
            )
          ))
        )
      },
      year_week = {
        switch(td$type,
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
        data[[f]] <- paste0(year, '-', sprintf("%02d", week))
      },
      week = {
        switch(td$type,
               iso = {
                 week <- lubridate::isoweek(values)
               },
               epi = {
                 week <- lubridate::epiweek(values)
               },
               {
                 week <- lubridate::week(values)
               })
        data[[f]] <- sprintf("%02d", week)
      },
      year_semester = {
        data[[f]] <-
          paste0(lubridate::year(values),
                 '-',
                 lubridate::semester(values))
      },
      semester = {
        data[[f]] <- as.character(lubridate::semester(values))
      },
      year_quarter = {
        data[[f]] <-
          paste0(lubridate::year(values),
                 '-',
                 lubridate::quarter(values))
      },
      quarter = {
        data[[f]] <- as.character(lubridate::quarter(values))
      },
      year_month = {
        data[[f]] <-
          paste0(lubridate::year(values),
                 '-',
                 sprintf("%02d", lubridate::month(values)))
      },
      month = {
        data[[f]] <- sprintf("%02d", lubridate::month(values))
      },
      month_name = {
        data[[f]] <- as.character(lubridate::month(
          values,
          label = TRUE,
          abbr = FALSE,
          locale = td$locale
        ))
      },
      month_num_name = {
        data[[f]] <-
          paste0(
            sprintf("%02d", lubridate::month(values)),
            '-',
            lubridate::month(
              values,
              label = TRUE,
              abbr = FALSE,
              locale = td$locale
            )
          )
      },
      month_abbr = {
        data[[f]] <- as.character(lubridate::month(
          values,
          label = TRUE,
          abbr = TRUE,
          locale = td$locale
        ))
      },
      month_num_abbr = {
        data[[f]] <-
          paste0(
            sprintf("%02d", lubridate::month(values)),
            '-',
            lubridate::month(
              values,
              label = TRUE,
              abbr = TRUE,
              locale = td$locale
            )
          )
      },
      year = {
        data[[f]] <- as.character(lubridate::year(values))
      },
      decade = {
        data[[f]] <- as.character(10 * (lubridate::year(values) %/% 10))
      },
      time = {
        data[[f]] <- values
      },
      hour = {
        data[[f]] <- substr(values, 1, 2)
      },
      minute = {
        data[[f]] <- substr(values, 4, 5)
      },
      second = {
        data[[f]] <- substr(values, 7, 8)
      },
      {
        stop(sprintf("Field '%s' is not considered.", f))
      }
    )
  }
  tibble::as_tibble(as.data.frame(data))
}
