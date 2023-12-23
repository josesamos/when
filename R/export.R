#' Get the table of the dimension
#'
#' Obtain the table of the defined dimension.
#'
#' @param td A `when` object.
#'
#' @return A `tibble`, the table.
#'
#' @family exporting results
#'
#' @examples
#'
#' table <- when() |>
#'   get_table()
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


#' #' Generate tables in a relational database
#' #'
#' #' Given a connection to a relational database, it stores the facts and
#' #' dimensions in the form of tables. Tables can be overwritten.
#' #'
#' #' @param td A `when` object.
#' #' @param con A `DBI::DBIConnection` object.
#' #' @param overwrite A boolean, allow overwriting tables in the database.
#' #'
#' #' @return Invisible NULL.
#' #'
#' #' @family exporting results
#' #'
#' #' @examples
#' #'
#' #' my_db <- DBI::dbConnect(RSQLite::SQLite())
#' #'
#' #' when() |>
#' #'   get_table_rdb(my_db)
#' #'
#' #' DBI::dbDisconnect(my_db)
#' #'
#' #' @export
#' get_table_rdb <- function(db, con, overwrite) UseMethod("get_table_rdb")
#'
#' #' @rdname get_table_rdb
#' #'
#' #' @export
#' get_table_rdb.star_database <- function(db, con, overwrite = FALSE) {
#'   if (overwrite) {
#'     tables <- DBI::dbListTables(con)
#'     dimensions <- get_dimension_names(db)
#'     facts <- get_fact_names(db)
#'     dimensions <- intersect(tables, dimensions)
#'     facts <- intersect(tables, facts)
#'     for (f in facts) {
#'       DBI::dbRemoveTable(con, f)
#'     }
#'     for (d in dimensions) {
#'       DBI::dbRemoveTable(con, d)
#'     }
#'   }
#'   db_dm <- as_dm_class(db)
#'   dm::copy_dm_to(con, db_dm, temporary = FALSE)
#'   invisible(NULL)
#' }
#'
#' #' Generate a xlsx file with fact and dimension tables
#' #'
#' #' To port databases to other work environments it is useful to be able to
#' #' export them as a xlsx file, as this function does.
#' #'
#' #' @param db A `star_database` object.
#' #' @param file A string, name of a file.
#' #'
#' #' @return A string, name of a file.
#' #'
#' #' @family exporting results
#' #'
#' #' @examples
#' #' \donttest{
#' #' db1 <- star_database(mrs_cause_schema, ft_num) |>
#' #'   snake_case()
#' #' tl1 <- db1 |>
#' #'   get_table_xlsx()
#' #'
#' #' db2 <- star_database(mrs_age_schema, ft_age) |>
#' #'   snake_case()
#' #'
#' #' ct <- constellation("MRS", db1, db2)
#' #' f <- ct |>
#' #'   get_table_xlsx(file = tempfile())
#' #' }
#' #' @export
#' get_table_xlsx <- function(db, file) UseMethod("get_table_xlsx")
#'
#' #' @rdname get_table_xlsx
#' #'
#' #' @export
#' get_table_xlsx.star_database <- function(db, file = NULL) {
#'   if (is.null(file)) {
#'     file <- tempfile()
#'   }
#'   file <- tools::file_path_sans_ext(file)
#'   file <- paste0(file, '.xlsx')
#'
#'   l <- as_tibble_list(db)
#'   names <- names(l)
#'   xlsx::write.xlsx(
#'     as.data.frame(l[[1]]),
#'     file = file,
#'     sheetName = names[1],
#'     row.names = FALSE,
#'     showNA = FALSE
#'   )
#'   if (length(l) > 1) {
#'     for (i in 2:length(l)) {
#'       xlsx::write.xlsx(
#'         as.data.frame(l[[i]]),
#'         file = file,
#'         sheetName = names[i],
#'         append = TRUE,
#'         row.names = FALSE,
#'         showNA = FALSE
#'       )
#'     }
#'   }
#'   file
#' }
#'
#'
#' #' Generate csv files with fact and dimension tables
#' #'
#' #' To port databases to other work environments it is useful to be able to
#' #' export them as csv files, as this function does.
#' #'
#' #' @param db A `star_database` object.
#' #' @param dir A string, name of a dir.
#' #' @param type An integer, 1: uses "." for the decimal point and a comma for the
#' #' separator; 2: uses a comma for the decimal point and a semicolon for the
#' #' separator.
#' #'
#' #' @return A string, name of a dir.
#' #'
#' #' @family exporting results
#' #'
#' #' @examples
#' #'
#' #' db1 <- star_database(mrs_cause_schema, ft_num) |>
#' #'   snake_case()
#' #' tl1 <- db1 |>
#' #'   get_table_csv()
#' #'
#' #' db2 <- star_database(mrs_age_schema, ft_age) |>
#' #'   snake_case()
#' #'
#' #' ct <- constellation("MRS", db1, db2)
#' #' d <- ct |>
#' #'   get_table_csv(dir = tempdir())
#' #'
#' #' @export
#' get_table_csv <- function(db, dir, type) UseMethod("get_table_csv")
#'
#' #' @rdname get_table_csv
#' #'
#' #' @export
#' get_table_csv.star_database <- function(db, dir = NULL, type = 1) {
#'   if (is.null(dir)) {
#'     dir <- tempdir()
#'   }
#'   l <- as_tibble_list(db)
#'   names <- names(l)
#'   for (i in seq_along(l)) {
#'     file <- paste0(dir, '/', names[i], '.csv')
#'     if (type == 1) {
#'       utils::write.csv(l[[i]], file = file, row.names = FALSE)
#'     } else {
#'       utils::write.csv2(l[[i]], file = file, row.names = FALSE)
#'     }
#'   }
#'   dir
#' }
#'
