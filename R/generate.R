#' Generate table
#'
#' Once all the characteristics of the dimension have been defined, we can generate
#' its table according to them using this function.
#'
#' @param td A `when` object.
#'
#' @return A `when` object.
#'
#' @family obtaining results
#' @seealso \code{\link{when}}, \code{\link{get_table}}
#'
#' @examples
#'
#' td <- when() |>
#'   generate_table()
#'
#' @importFrom rlang :=
#'
#' @export
generate_table <-
  function(td)
    UseMethod("generate_table")

#' @rdname generate_table
#'
#' @export
generate_table.when <-
  function(td) {
    if (is.null(td$table_name)) {
      if (td$type == 'time') {
        td$table_name = 'time'
      } else {
        td$table_name = 'date'
      }
    }
    fields <- get_fields(td)
    values <- get_values(td)
    data <- get_data(td, values, fields)
    if (td$surrogate_key) {
      surrogate_key <- "id"
      data <-
        tibble::add_column(data, !!surrogate_key := 1:nrow(data), .before = 1)
    }
    td$table <- data
    td
  }


#' Get level names
#'
#' Returns the names of the levels. We can obtain all the available ones or only
#' the selected ones.
#'
#' @param td A `when` object.
#' @param selected A boolean.
#'
#' @return A string vector.
#'
#' @family obtaining results
#'
#' @examples
#'
#' names <- when() |>
#'   get_level_names()
#'
#' @export
get_level_names <-
  function(td, selected)
    UseMethod("get_level_names")

#' @rdname get_level_names
#'
#' @export
get_level_names.when <-
  function(td, selected = FALSE) {
    if (td$type == 'time') {
      names <- 'time'
    } else {
      names <- names(td$level_type[td$level_type == 'date'])
      if (selected) {
        names <- names[td$level_include_conf[names]]
      }
    }
    names
  }


#' Get level attribute names
#'
#' Returns the names of the level attributes. We can obtain all the available ones
#' or only the selected ones.
#'
#' @param td A `when` object.
#' @param name A string.
#' @param selected A boolean.
#'
#' @return A string vector.
#'
#' @family obtaining results
#'
#' @examples
#'
#' names <- when() |>
#'   get_level_attribute_names()
#'
#' @export
get_level_attribute_names <-
  function(td, name, selected)
    UseMethod("get_level_attribute_names")

#' @rdname get_level_attribute_names
#'
#' @export
get_level_attribute_names.when <-
  function(td, name = NULL, selected = FALSE) {
    if (td$type == 'time') {
      names <- names(td$att_levels[td$att_levels == 'time'])
    } else {
      if (is.null(name)) {
        name <- names(td$level_type[td$level_type == 'date'])
      }
      stopifnot("'name' must be a date level." = all(name %in% names(td$level_type[td$level_type == 'date'])))
      names <- names(td$att_levels[td$att_levels %in% name])
    }
    if (selected) {
      names <- names[td$att_include_conf[names]]
      table_names <- get_fields(td)
      names <- intersect(names, table_names)
    }
    names
  }


#' Set table attribute names
#'
#' Rename the attributes of the dimension table. It is especially useful if we want
#' to export the table, for example, to a database.
#'
#' @param td A `when` object.
#' @param names A string vector.
#'
#' @return A `when` object.
#'
#' @family obtaining results
#'
#' @examples
#'
#' wd <- when() |>
#'   generate_table()
#' wd |>
#'   get_table_attribute_names()
#'
#' wd <- wd |>
#'   set_table_attribute_names(
#'     c(
#'       'id_when',
#'       'date',
#'       'month_day',
#'       'week_day',
#'       'day_name',
#'       'day_num_name',
#'       'year_week',
#'       'week',
#'       'year_month',
#'       'month',
#'       'month_name',
#'       'month_num_name',
#'       'year'
#'     )
#'   )
#'
#' @export
set_table_attribute_names <-
  function(td, names)
    UseMethod("set_table_attribute_names")

#' @rdname set_table_attribute_names
#'
#' @export
set_table_attribute_names.when <-
  function(td, names = NULL) {
    stopifnot("There are repeated attribute names." = length(names) == length(unique(names)))
    stopifnot("The table has a different number of attributes." = length(names(td$table)) == length(names))
    td$attribute_names <- names
    td
  }


#' Get table attribute names
#'
#' Returns the names of the dimension table attributes as a string vector or in
#' string form, so we can easily use it to rename them if deemed necessary.
#'
#' If the table has not been generated yet, returns the attributes it will contain
#' when it is generated.
#'
#' @param td A `when` object.
#' @param as_string A boolean.
#'
#' @return A string.
#'
#' @family obtaining results
#'
#' @examples
#'
#' names <- when() |>
#'   get_table_attribute_names()
#'
#' @export
get_table_attribute_names <-
  function(td, as_string)
    UseMethod("get_table_attribute_names")

#' @rdname get_table_attribute_names
#'
#' @export
get_table_attribute_names.when <-
  function(td, as_string = TRUE) {
    if (length(names(td$table)) == length(td$attribute_names)) {
      names <- td$attribute_names
    } else {
      names <- names(td$table)
    }
    if (length(names) == 0) {
      names <- get_fields(td)
      if (td$surrogate_key) {
        names <- c('id', names)
      }
    }
    if (as_string) {
      dt <- "c("
      for (j in seq_along(names)) {
        if (j == 1) {
          sep = ""
        } else {
          sep = ", "
        }
        dt <- paste(dt, sprintf("'%s'", names[j]), sep = sep)
      }
      dt <- paste(dt, ")", sep = "")
      names <- dt
    }
    names
  }


#' Get the fields according to the defined configuration
#'
#' @param td A `when` object.
#'
#' @return A vector of string.
#'
#' @keywords internal
get_fields <- function(td) {
  levels <- names(td$level_include_conf[td$level_include_conf])
  fields <- names(td$att_levels[td$att_levels %in% levels])
  fields <- fields[td$att_include_conf[fields]]
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
    if (td$type == 'time') {
      if (td$start == td$end) {
        td$start <- "00:00:00"
        td$end <- "23:59:59"
      }
      values <-
        time_seconds[time_seconds >= as.character(td$start) &
                       time_seconds <= as.character(td$end)]
    } else {
      values <-
        date_days[date_days >= as.character(td$start) &
                    date_days <= as.character(td$end)]
      if (td$level_include_conf['day'] |
          td$level_include_conf['week']) {
        inc <- lubridate::days(1)
      } else if (td$level_include_conf['month'] |
                 td$level_include_conf['quarter'] |
                 td$level_include_conf['semester']) {
        inc <- base::months(1)
        td$start <- paste0(substr(td$start, 1, 8), "01")
        td$end <- paste0(substr(td$end, 1, 8), "01")
      } else if (td$level_include_conf['year']) {
        inc <- lubridate::years(1)
        td$start <- paste0(substr(td$start, 1, 5), "01-01")
        td$end <- paste0(substr(td$end, 1, 5), "01-01")
      }

      if (td$start < date_days[1]) {
        val <- lubridate::ymd(td$start)
        while (val < date_days[1]) {
          values <- c(values, as.character(val))
          val <- lubridate::ymd(val) + inc
        }
      }
      if (td$end > date_days[length(date_days)]) {
        val <- lubridate::ymd(date_days[length(date_days)]) + inc
        while (val <= td$end) {
          values <- c(values, as.character(val))
          val <- lubridate::ymd(val) + inc
        }
      }
    }
  }
  if (td$type == 'time') {
    if (td$att_include_conf['minute']) {
      if (!td$att_include_conf['second']) {
        values <- paste0(substr(values, 1, 6), "00")
      }
    } else {
      values <- paste0(substr(values, 1, 3), "00:00")
    }
  } else {
    if (!td$level_include_conf['day'] &
        !td$level_include_conf['week']) {
      if (td$level_include_conf['month'] |
          td$level_include_conf['quarter'] |
          td$level_include_conf['semester']) {
        values <- paste0(substr(values, 1, 8), "01")
      } else if (td$level_include_conf['year']) {
        values <- paste0(substr(values, 1, 5), "01-01")
      }
    }
  }
  sort(unique(values))
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
    data <-
      td$att_function[[f]](
        data,
        values,
        type = td$type,
        locale = td$locale,
        week_starts_monday = td$week_starts_monday,
        day_part = td$day_part
      )
  }
  data <- tibble::as_tibble(as.data.frame(data))
  unique(data)
}
