#' Configure instances
#'
#' Configure instances.
#'
#' @param td A `when` object.
#' @param start A string, start of the period to be included in the dimension.
#' @param end A string, end of the period to be included in the dimension.
#' @param values A vector of string.
#'
#' @return A `when` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- when() |>
#'   configure_instances()
#'
#' @export
configure_instances <-
  function(td, start, end, values)
    UseMethod("configure_instances")

#' @rdname configure_instances
#'
#' @export
configure_instances.when <-
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
