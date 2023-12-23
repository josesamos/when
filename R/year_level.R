
#' Include year level
#'
#' Include year level.
#'
#' @param td A `timedimension` object.
#' @param year_level A boolean, include year level.
#' @param include_year A boolean, include the year field.
#' @param include_decade A boolean, include the decade field.
#'
#' @return A `timedimension` object.
#'
#' @family time definition
#'
#' @examples
#'
#' td <- timedimension() |>
#'   include_year_level()
#'
#' @export
include_year_level <-
  function(td, year_level, include_year, include_decade)
    UseMethod("include_year_level")

#' @rdname include_year_level
#'
#' @export
include_year_level.timedimension <-
  function(td,
           year_level = TRUE,
           include_year = TRUE,
           include_decade = TRUE) {
    stopifnot("'year_level' must be of logical type." = is.logical(year_level))
    stopifnot("'include_year' must be of logical type." = is.logical(include_year))
    stopifnot("'include_decade' must be of logical type." = is.logical(include_decade))
    td$year_level <- year_level
    td$include_year <- include_year
    td$include_decade <- include_decade
    td
  }

