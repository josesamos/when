#' Include surrogate_key
#'
#' Include surrogate_key.
#'
#' @param td A `when` object.
#' @param surrogate_key A boolean, include a surrogate key in the table.
#'
#' @return A `when` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_surrogate_key()
#'
#' @export
include_surrogate_key <-
  function(td, surrogate_key)
    UseMethod("include_surrogate_key")

#' @rdname include_surrogate_key
#'
#' @export
include_surrogate_key.when <-
  function(td,
           surrogate_key = TRUE) {
    if (!is.null(surrogate_key)) {
      stopifnot("'surrogate_key' must be of logical type." = is.logical(surrogate_key))
      td$surrogate_key <- surrogate_key
    }
    td
  }
