#' Get the table of the dimension
#'
#' Once all the configuration elements have been defined and the dimension table
#' has been generated, using this function we can obtain it in `tibble` format.
#'
#' @param td A `when` object.
#'
#' @return A `tibble`, the table.
#'
#' @family getting results
#' @seealso \code{\link{when}}, \code{\link{generate_table}}
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
    if (length(names(table)) == length(td$attribute_names)) {
      names(table) <- td$attribute_names
    }
    table
  }


#' Store the table in a relational database
#'
#' Once all the configuration elements have been defined and the dimension table
#' has been generated, using this function we can obtain it in table format in a
#' Relational DBMS.
#'
#' @param td A `when` object.
#' @param con A `DBI::DBIConnection` object.
#' @param overwrite A boolean, allow overwriting tables in the database.
#'
#' @return Invisible NULL.
#'
#' @family getting results
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
#' Once all the configuration elements have been defined and the dimension table
#' has been generated, using this function we can obtain it in *xlsx* format.
#'
#' If no dir name is given, stores the table in a temporary one.
#'
#' @param td A `when` object.
#' @param dir A string, name of a dir.
#'
#' @return A string, name of a file.
#'
#' @family getting results
#'
#' @examples
#'
#' file <- when() |>
#'   generate_table() |>
#'   get_table_xlsx()
#'
#' @export
get_table_xlsx <- function(td, dir)
  UseMethod("get_table_xlsx")

#' @rdname get_table_xlsx
#'
#' @export
get_table_xlsx.when <- function(td, dir = NULL) {
  if (is.null(dir)) {
    dir <- tempdir()
  }
  nexus <- get_nexus(dir)
  file <- paste0(dir, nexus, td$table_name, '.xlsx')
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
#' Once all the configuration elements have been defined and the dimension table
#' has been generated, using this function we can obtain it in csv format.
#'
#' If no dir name is given, stores the table in a temporary one.
#'
#' @param td A `when` object.
#' @param dir A string, name of a dir.
#' @param type An integer, 1: uses "." for the decimal point and a comma for the
#' separator; 2: uses a comma for the decimal point and a semicolon for the
#' separator.
#'
#' @return A string, name of a file.
#'
#' @family getting results
#'
#' @examples
#'
#' file <- when() |>
#'   generate_table() |>
#'   get_table_csv()
#'
#' @export
get_table_csv <- function(td, dir, type)
  UseMethod("get_table_csv")

#' @rdname get_table_csv
#'
#' @export
get_table_csv.when <- function(td, dir = NULL, type = 1) {
  if (is.null(dir)) {
    dir <- tempdir()
  }
  nexus <- get_nexus(dir)
  file <- paste0(dir, nexus, td$table_name, '.csv')
  table <- get_table(td)
  if (type == 1) {
    utils::write.csv(table, file = file, row.names = FALSE)
  } else {
    utils::write.csv2(table, file = file, row.names = FALSE)
  }
  file
}


#' Get nexus
#'
#' Given a name, if it ends in "/" the nexus is the empty string, otherwise it
#' is "/".
#'
#' @param name A string.
#'
#' @return A string.
#'
#' @keywords internal
get_nexus <- function(name) {
  l <- nchar(name)
  c <- substr(name, start = l, stop = l)
  if (c == "/") {
    nexus <- ""
  } else {
    nexus <- "/"
  }
  nexus
}
