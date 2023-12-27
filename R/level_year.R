#' Include year level
#'
#' When the dimension is defined as a date type, using this function we can select
#' the year level and its attributes to include in it: year and decade.
#'
#' @param td A `when` object.
#' @param level A boolean, include year level.
#' @param include_all A boolean, include all fields of the level.
#' @param year A boolean, include the year field.
#' @param decade A boolean, include the decade field.
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
           level,
           include_all,
           year,
           decade)
    UseMethod("include_year_level")

#' @rdname include_year_level
#'
#' @export
include_year_level.when <-
  function(td,
           level = TRUE,
           include_all = FALSE,
           year = TRUE,
           decade = FALSE) {
    stopifnot("'level' must be of logical type." = is.logical(level))
    stopifnot("'include_all' must be of logical type." = is.logical(include_all))
    stopifnot("'year' must be of logical type." = is.logical(year))
    stopifnot("'decade' must be of logical type." = is.logical(decade))
    td$year_level <- level
    if (include_all) {
      td$include_year <- TRUE
      td$include_decade <- TRUE
    } else {
      td$include_year <- year
      td$include_decade <- decade
    }
    td
  }
