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
#'   generate_table() |>
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
    stopifnot("The table must be previously generated using the `generate_table()` function." = !is.null(td$table))
    table <- td$table
    if (!is.null(td$attribute_names)) {
      names(table) <- td$attribute_names
    }
    table
  }


#' Set attribute names
#'
#' @param td A `when` object.
#' @param names A string vector.
#'
#' @return A `when` object.
#'
#' @family exporting results
#'
#' @examples
#'
#' table <- when() |>
#'   set_attribute_names()
#'
#' @export
set_attribute_names <-
  function(td, names)
    UseMethod("set_attribute_names")

#' @rdname set_attribute_names
#'
#' @export
set_attribute_names.when <-
  function(td, names = NULL) {
    stopifnot("There are repeated attribute names." = length(names) == length(unique(names)))
    td$td$attribute_names <- names
    td
  }


#' Store the table in a relational database
#'
#' @param td A `when` object.
#' @param con A `DBI::DBIConnection` object.
#' @param overwrite A boolean, allow overwriting tables in the database.
#'
#' @return Invisible NULL.
#'
#' @family exporting results
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite())
#'
#' when() |>
#'   generate_table() |>
#'   get_table_rdb(my_db)
#'
#' DBI::dbDisconnect(my_db)
#'
#' @export
get_table_rdb <-
  function(td, con, overwrite)
    UseMethod("get_table_rdb")

#' @rdname get_table_rdb
#'
#' @export
get_table_rdb.when <- function(td, con, overwrite = FALSE) {
  if (overwrite) {
    tables <- DBI::dbListTables(con)
    del <- intersect(tables, td$table_name)
    for (t in del) {
      DBI::dbRemoveTable(con, t)
    }
  }
  table <- get_table(td)
  db_dm <- dm::dm() |>
    dm::dm(!!td$table_name := table)

  if (td$surrogate_key) {
    key <- names(table)[1]
    db_dm <- db_dm |>
      dm::dm_add_pk(!!td$table_name,!!key)
  }

  dm::copy_dm_to(con, db_dm, temporary = FALSE)
  invisible(NULL)
}


#' Store the table in a xlsx file
#'
#' @param td A `when` object.
#' @param file A string, name of a file.
#'
#' @return A string, name of a file.
#'
#' @family exporting results
#'
#' @examples
#'
#' file <- when() |>
#'   generate_table() |>
#'   get_table_xlsx()
#'
#' @export
get_table_xlsx <- function(td, file)
  UseMethod("get_table_xlsx")

#' @rdname get_table_xlsx
#'
#' @export
get_table_xlsx.when <- function(td, file = NULL) {
  if (is.null(file)) {
    file <- paste0(tempdir(), '/', td$table_name, '.xlsx')
  }
  file <- tools::file_path_sans_ext(file)
  file <- paste0(file, '.xlsx')

  table <- get_table(td)
  xlsx::write.xlsx(
    as.data.frame(table),
    file = file,
    sheetName = td$table_name,
    row.names = FALSE,
    showNA = FALSE
  )
  file
}


#' Store the table in a csv files
#'
#' @param td A `when` object.
#' @param file A string, name of a file.
#' @param type An integer, 1: uses "." for the decimal point and a comma for the
#' separator; 2: uses a comma for the decimal point and a semicolon for the
#' separator.
#'
#' @return A string, name of a file.
#'
#' @family exporting results
#'
#' @examples
#'
#' file <- when() |>
#'   generate_table() |>
#'   get_table_csv()
#'
#' @export
get_table_csv <- function(td, file, type)
  UseMethod("get_table_csv")

#' @rdname get_table_csv
#'
#' @export
get_table_csv.when <- function(td, file = NULL, type = 1) {
  if (is.null(file)) {
    file <- paste0(tempdir(), '/', td$table_name, '.csv')
  }
  file <- tools::file_path_sans_ext(file)
  file <- paste0(file, '.csv')

  table <- get_table(td)
  if (type == 1) {
    utils::write.csv(table, file = file, row.names = FALSE)
  } else {
    utils::write.csv2(table, file = file, row.names = FALSE)
  }
  file
}
