#' Include surrogate_key
#'
#' Include a generated key in the dimension table. The default name is 'id'. Later
#' we can rename all the fields in the dimension table.
#'
#' @param td A `when` object.
#' @param surrogate_key A boolean, include a surrogate key in the dimension table.
#'
#' @return A `when` object.
#'
#' @family dimension definition
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
