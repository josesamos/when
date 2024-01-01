#' Get week date range
#'
#' For weeks between the given dates, gets the date of the first and last day of
#' each week.
#'
#' @param start A string, start of the period to be included in the dimension.
#' @param end A string, end of the period to be included in the dimension.
#' @param type A string, type of calendar (NULL, 'iso', 'epi' or 'time').
#'
#' @return A `tibble`.
#'
#' @family dimension definition
#'
#' @examples
#'
#' t <- get_week_date_range(start = "2024-01-01", end = "2029-12-31")
#'
#' @export
get_week_date_range <-
  function(start = NULL,
           end = NULL,
           type = NULL) {
    if (is.null(type)) {
      type = 'date'
    }
    type <- snakecase::to_snake_case(type)
    stopifnot("'type' does not have one of the allowed values." = type %in% c('date', 'iso', 'epi'))
    nstart <- as.character(lubridate::ymd(start) - 6)
    nend <- as.character(lubridate::ymd(end) + 6)
    td <- when(type = type, start = nstart, end = nend)
    values <- get_values(td)
    data <- get_data(td, values, fields = c("date", "year_week"))
    data_first <- data |>
      dplyr::group_by_at(dplyr::vars("year_week")) |>
      dplyr::summarise(first = min(date)) |>
      dplyr::arrange(dplyr::vars("first"))
    data_last <- data |>
      dplyr::group_by_at(dplyr::vars("year_week")) |>
      dplyr::summarise(last = max(date)) |>
      dplyr::arrange(dplyr::vars("last"))
    data <- dplyr::inner_join(data_first, data_last, by = "year_week")
    while (data$last[[1]] < start) {
      data <- data[-1, ]
    }
    l <- nrow(data)
    while (data$first[[l]] > end) {
      data <- data[-l, ]
      l <- nrow(data)
    }
    data
  }


