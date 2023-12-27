#' Include year level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the year level and its attributes to include in it: year and decade.
#'
#' @param td A `when` object.
#' @param year_level A boolean, include year level.
#' @param include_all A boolean, include all fields of the level.
#' @param include_year A boolean, include the year field.
#' @param include_decade A boolean, include the decade field.
#'
#' @return A `when` object.
#'
#' @family dimension definition
#'
#' @examples
#'
#' td <- when() |>
#'   include_year_level(include_decade = FALSE)
#'
#' @export
include_year_level <-
  function(td,
           year_level,
           include_all,
           include_year,
           include_decade)
    UseMethod("include_year_level")

#' @rdname include_year_level
#'
#' @export
include_year_level.when <-
  function(td,
           year_level = TRUE,
           include_all = FALSE,
           include_year = TRUE,
           include_decade = FALSE) {
    stopifnot("'year_level' must be of logical type." = is.logical(year_level))
    stopifnot("'include_all' must be of logical type." = is.logical(include_all))
    stopifnot("'include_year' must be of logical type." = is.logical(include_year))
    stopifnot("'include_decade' must be of logical type." = is.logical(include_decade))
    td$year_level <- year_level
    if (include_all) {
      td$include_year <- TRUE
      td$include_decade <- TRUE
    } else {
      td$include_year <- include_year
      td$include_decade <- include_decade
    }
    td
  }
