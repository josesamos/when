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
#'   get_table()
#'
#' @importFrom rlang :=
#'
#' @export
get_table <-
  function(td)
    UseMethod("get_table")

#' @rdname get_table
#'
#' @export
get_table.when <-
  function(td) {
    td$table
  }


