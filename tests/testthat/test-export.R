test_that("export", {
  td <- when(type = 'time') |>
    generate_table()

  td_1 <- td |>
    set_table_attribute_names(c('id1', 'time1', 'hour1', 'minute1', 'second1', 'day_part1'))

  t <- td |>
    get_table()

  t_1 <- td_1 |>
    get_table()

  my_db <- DBI::dbConnect(RSQLite::SQLite())
  when() |>
    generate_table() |>
    get_table_rdb(my_db)
  tables <- DBI::dbListTables(my_db)
  DBI::dbDisconnect(my_db)

  file_1 <- when() |>
    generate_table() |>
    get_table_xlsx()

  file_2 <- when() |>
    generate_table() |>
    get_table_csv()

  file_3 <- when() |>
    generate_table() |>
    get_table_csv(type = 2)

  expect_equal(
    names(t),
    c('id', 'time', 'hour', 'minute', 'second', 'day_part')
  )

  expect_equal(
    names(t_1),
    c('id1', 'time1', 'hour1', 'minute1', 'second1', 'day_part1')
  )

  expect_equal(
    tables,
    "when"
  )

  expect_equal(
    basename(file_1),
    "when.xlsx"
  )

  expect_equal(
    basename(file_2),
    "when.csv"
  )

  expect_equal(
    basename(file_3),
    "when.csv"
  )


})
