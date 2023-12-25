#' Define instances
#'
#' Using this function we can define the instances from which the dimension will
#' be generated according to the rest of its defined characteristics.
#'
#' We must indicate dates or date components in ISO 8601 format (yyyy-mm-dd). The
#' times in hh:mm:ss format.
#'
#' @param td A `when` object.
#' @param start A string, start of the period to be included in the dimension.
#' @param end A string, end of the period to be included in the dimension.
#' @param values A vector of string.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td_1 <- when() |>
#'   define_instances(start = "2020", end = "2030")
#'
#' td_1 <- when() |>
#'   define_instances(start = "2020-01-01", end = "2030-01-01")
#'
#' td_2 <- when(type = 'time') |>
#'   define_instances(values = 1:5)
#'
#' @export
define_instances <-
  function(td, start, end, values)
    UseMethod("define_instances")

#' @rdname define_instances
#'
#' @export
define_instances.when <-
  function(td,
           start = NULL,
           end = NULL,
           values = NULL) {
    if (is.null(start) & is.null(end)) {
      td$start <- NULL
      td$end <- NULL
    }
    if (is.null(values)) {
      td$values <- NULL
    }
    td <- validate_start_end(td, start, end)
    td <- validate_values(td, values)
    td
  }
