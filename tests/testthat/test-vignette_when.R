test_that("vignette_when", {
  w_date <- when()


  ## ---------------------------------------------------------------------------------------------------------
  w_time <- when(type = 'time')


  ## ---------------------------------------------------------------------------------------------------------
  w_time_2 <- when() |>
    define_characteristics(type = 'time')

  i1 <- identical(w_time, w_time_2)


  ## ---------------------------------------------------------------------------------------------------------
  w_date <- w_date |>
    define_characteristics(locale = Sys.setlocale("LC_TIME", "English"))


  ## ---------------------------------------------------------------------------------------------------------
  w_date <- w_date |>
    define_instances(start = lubridate::today(),
                     end = lubridate::today() + lubridate::years(5))


  ## ---------------------------------------------------------------------------------------------------------
  w_date_2_1 <-
    when(
      values = c(
        "2023-12-31",
        "2023-01-01",
        "2022-12-31",
        "2022-01-01",
        "2021-12-31",
        "2021-01-01"
      )
    )

  w_date_2_2 <- w_date |>
    define_instances(values = 2020:2030)


  ## ---------------------------------------------------------------------------------------------------------
  w_date_3 <- w_date |>
    define_instances(start = 2020, end = 2030)


  ## ---------------------------------------------------------------------------------------------------------
  w_date_4 <- w_date |>
    define_instances(start = "2020-01-01", end = "2030-01-01")

  i2 <- identical(w_date_3, w_date_4)


  ## ---------------------------------------------------------------------------------------------------------
  w_time_3 <- w_time |>
    define_instances(start = "00:00:00", end = "23:59:59")

  i3 <- identical(w_time, w_time_3)


  ## ---------------------------------------------------------------------------------------------------------
  w_time_4 <- w_time |>
    define_instances(start = 8, end = 17)

  w_time_5 <- w_time |>
    define_instances(start = "08:00:00", end = "17:00:00")

  i4 <- identical(w_time_4, w_time_5)


  ## ---------------------------------------------------------------------------------------------------------
  n1 <- w_date |>
    get_level_attribute_names(selected = TRUE)

  n2 <- w_date |>
    get_level_names()

  n3 <- w_date |>
    get_level_attribute_names(name = 'month', selected = TRUE)

  n4 <- w_date |>
    get_level_attribute_names(name = 'month')


  ## ---------------------------------------------------------------------------------------------------------
  n5 <- w_time |>
    get_level_attribute_names()

  n6 <- w_time |>
    get_level_names()


  ## ---------------------------------------------------------------------------------------------------------
  w_date_5 <- w_date |>
    select_month_level(month_name = FALSE)

  w_date_6 <- when(
    start = lubridate::today(),
    end = lubridate::today() + lubridate::years(5),
    month_name = FALSE
  )

  i5 <- identical(w_date_5, w_date_6)

  n7 <- w_date_5 |>
    get_level_attribute_names(name = 'month', selected = TRUE)


  ## ---------------------------------------------------------------------------------------------------------
  w_date_7 <- w_date |>
    select_month_level(exclude_all = TRUE, month_name = TRUE)

  n8 <- w_date_7 |>
    get_level_attribute_names(name = 'month', selected = TRUE)


  ## ---------------------------------------------------------------------------------------------------------
  w_date_8 <- w_date |>
    select_date_levels(month_level = FALSE)

  n9 <- w_date_8 |>
    get_level_attribute_names(name = 'month', selected = TRUE)


  ## ---------------------------------------------------------------------------------------------------------
  w_date_9 <- when(
    start = lubridate::today(),
    end = lubridate::today() + lubridate::years(5),
    month_level = FALSE
  )


  ## ---------------------------------------------------------------------------------------------------------
  n10 <- w_time |>
    get_level_names()


  ## ---------------------------------------------------------------------------------------------------------
  w_time_6 <- w_time |>
    select_time_level(exclude_all = TRUE)

  n11 <- w_time_6 |>
    get_level_attribute_names(selected = TRUE)

  w_time_7 <- w_time |>
    select_time_level(minute = FALSE)

  n12 <- w_time_7 |>
    get_level_attribute_names(selected = TRUE)


  ## ---------------------------------------------------------------------------------------------------------
  n13 <- w_date |>
    get_table_attribute_names(as_string = FALSE)


  ## ---------------------------------------------------------------------------------------------------------
  w_date <- w_date |>
    generate_table()

  w_time <- w_time |>
    generate_table()


  ## ---------------------------------------------------------------------------------------------------------
  t_date <- w_date |>
    get_table()

  t1 <- rbind(head(t_date, 5), tail(t_date, 5))


  ## ---------------------------------------------------------------------------------------------------------
  t_date <- w_date |>
    select_date_levels(day_level = FALSE) |>
    select_week_level(include_all = TRUE) |>
    generate_table() |>
    get_table()


  ## ----results = "asis"-------------------------------------------------------------------------------------

  t2 <- rbind(head(t_date, 5), tail(t_date, 5))

  ## ---------------------------------------------------------------------------------------------------------
  t_time <- w_time |>
    get_table()


  ## ----results = "asis"-------------------------------------------------------------------------------------
  t3 <- rbind(head(t_time, 5), tail(t_time, 5))

  ## ---------------------------------------------------------------------------------------------------------
  t_time <- w_time |>
    select_time_level(second = FALSE) |>
    generate_table() |>
    get_table()


  ## ----results = "asis"-------------------------------------------------------------------------------------
  t4 <- rbind(head(t_time, 5), tail(t_time, 5))

  ## ----database---------------------------------------------------------------------------------------------
  my_db <- DBI::dbConnect(RSQLite::SQLite())

  w_date |>
    get_table_rdb(my_db)

  w_time |>
    get_table_rdb(my_db)

  tables <- DBI::dbListTables(my_db)

  DBI::dbDisconnect(my_db)


  ## ---------------------------------------------------------------------------------------------------------
  wd_1 <- when(name = 'dim_where')

  wd_2 <- when() |>
    define_characteristics(name = 'dim_where')


  ## ---------------------------------------------------------------------------------------------------------
  my_db <- DBI::dbConnect(RSQLite::SQLite())

  wd_1 |>
    generate_table() |>
    get_table_rdb(my_db)

  n14 <- DBI::dbListTables(my_db)

  DBI::dbDisconnect(my_db)


  ## ---------------------------------------------------------------------------------------------------------
  n15 <- wd_2 |>
    generate_table() |>
    get_table_csv()
  n15 <- basename(n14)


  ## ---------------------------------------------------------------------------------------------------------
  n16 <- when() |>
    get_table_attribute_names(as_string = FALSE)

  n17 <- when(surrogate_key = FALSE) |>
    get_table_attribute_names(as_string = FALSE)


  ## ---------------------------------------------------------------------------------------------------------
  wd_3 <- when() |>
    generate_table()


  ## ---------------------------------------------------------------------------------------------------------
  n18 <- wd_3 |>
    get_table_attribute_names()

  wd_3 <- wd_3 |>
    set_table_attribute_names(
      c(
        'id_when',
        'date',
        'month_day',
        'week_day',
        'day_name',
        'day_num_name',
        'year_week',
        'week',
        'year_month',
        'month',
        'month_name',
        'month_num_name',
        'year'
      )
    )

  n19 <- wd_3 |>
    get_table_attribute_names(as_string = FALSE)


  ## ---------------------------------------------------------------------------------------------------------
  n20 <- when() |>
    get_day_part()

  n21 <- when() |>
    set_day_part(hour = c(20:23, 0:5), name = "Night") |>
    set_day_part(hour = c(6:19), name = "Day") |>
    get_day_part()


  ## ---------------------------------------------------------------------------------------------------------
  wd_1 <- when(week_starts_monday = FALSE)

  wd_2 <- when() |>
    define_characteristics(week_starts_monday = FALSE)


  ## ---------------------------------------------------------------------------------------------------------
  wd <- when()

  f1 <- wd |>
    get_attribute_definition_function(name = "year")

  f2 <- wd |>
    get_attribute_definition_function(name = "year_week")


  ## ---------------------------------------------------------------------------------------------------------
  f <- function(table, values, ...) {
    dots <- list(...)
    type <- dots[['type']]
    table[['year']] <- as.character(lubridate::year(values))
    if (type == 'iso') {
      table[['week_year']] <- as.character(lubridate::isoyear(values))
    } else if (type == 'epi') {
      table[['week_year']] <- as.character(lubridate::epiyear(values))
    }
    table
  }

  wd <- wd |>
    set_attribute_definition_function(name = "year", f)


  ## ---------------------------------------------------------------------------------------------------------
  t <- wd |>
    define_characteristics(type = 'iso') |>
    generate_table() |>
    get_table()

  t5 <- names(t)


  expect_equal(i1, TRUE)

  expect_equal(i2, TRUE)

  expect_equal(i3, TRUE)

  expect_equal(i4, TRUE)

  expect_equal(i5, TRUE)

  expect_equal(
    names(t1),
    c(
      "id",
      "date",
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "year_week",
      "week",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(
    names(t2),
    c(
      "id",
      "year_week",
      "week",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(t3, structure(
    list(
      id = c(1L, 2L, 3L, 4L, 5L, 86396L, 86397L, 86398L,
             86399L, 86400L),
      time = c(
        "00:00:00",
        "00:00:01",
        "00:00:02",
        "00:00:03",
        "00:00:04",
        "23:59:55",
        "23:59:56",
        "23:59:57",
        "23:59:58",
        "23:59:59"
      ),
      hour = c("00", "00", "00", "00", "00", "23", "23",
               "23", "23", "23"),
      minute = c("00", "00", "00", "00", "00", "59",
                 "59", "59", "59", "59"),
      second = c("00", "01", "02", "03", "04",
                 "55", "56", "57", "58", "59"),
      day_part = c(
        "Night",
        "Night",
        "Night",
        "Night",
        "Night",
        "Night",
        "Night",
        "Night",
        "Night",
        "Night"
      )
    ),
    row.names = c(NA,-10L),
    class = c("tbl_df", "tbl",
              "data.frame")
  ))

  expect_equal(t4, structure(
    list(
      id = c(1L, 2L, 3L, 4L, 5L, 1436L, 1437L, 1438L,
             1439L, 1440L),
      time = c(
        "00:00:00",
        "00:01:00",
        "00:02:00",
        "00:03:00",
        "00:04:00",
        "23:55:00",
        "23:56:00",
        "23:57:00",
        "23:58:00",
        "23:59:00"
      ),
      hour = c("00", "00", "00", "00", "00", "23", "23", "23", "23",
               "23"),
      minute = c("00", "01", "02", "03", "04", "55", "56", "57",
                 "58", "59"),
      day_part = c(
        "Night",
        "Night",
        "Night",
        "Night",
        "Night",
        "Night",
        "Night",
        "Night",
        "Night",
        "Night"
      )
    ),
    row.names = c(NA,-10L),
    class = c("tbl_df", "tbl", "data.frame")
  ))

  expect_equal(
    t5,
    c(
      "id",
      "date",
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "year_week",
      "week",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year",
      "week_year"
    )
  )

  expect_equal(
    n1,
    c(
      "date",
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "year_week",
      "week",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(n2, c("day", "week", "month", "quarter", "semester", "year"))

  expect_equal(n3, c("year_month", "month", "month_name", "month_num_name"))

  expect_equal(
    n4,
    c(
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "month_abbr",
      "month_num_abbr"
    )
  )

  expect_equal(n5, c("time", "hour", "minute", "second", "day_part"))

  expect_equal(n6, "time")

  expect_equal(n7, c("year_month", "month", "month_num_name"))

  expect_equal(n8, "month_name")

  expect_equal(n9, character(0))

  expect_equal(n10, "time")

  expect_equal(n11, "hour")

  expect_equal(n12, c("time", "hour", "day_part"))

  expect_equal(
    n13,
    c(
      "id",
      "date",
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "year_week",
      "week",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(n14, "dim_where")

  expect_equal(n15, "dim_where")

  expect_equal(
    n16,
    c(
      "id",
      "date",
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "year_week",
      "week",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(
    n17,
    c(
      "date",
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "year_week",
      "week",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(
    n18,
    "c('id', 'date', 'month_day', 'week_day', 'day_name', 'day_num_name', 'year_week', 'week', 'year_month', 'month', 'month_name', 'month_num_name', 'year')"
  )

  expect_equal(
    n19,
    c(
      "id_when",
      "date",
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "year_week",
      "week",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(
    n20,
    c(
      `00` = "Night",
      `01` = "Night",
      `02` = "Night",
      `03` = "Night",
      `04` = "Night",
      `05` = "Morning",
      `06` = "Morning",
      `07` = "Morning",
      `08` = "Morning",
      `09` = "Morning",
      `10` = "Morning",
      `11` = "Morning",
      `12` = "Afternoon",
      `13` = "Afternoon",
      `14` = "Afternoon",
      `15` = "Afternoon",
      `16` = "Afternoon",
      `17` = "Evening",
      `18` = "Evening",
      `19` = "Evening",
      `20` = "Evening",
      `21` = "Night",
      `22` = "Night",
      `23` = "Night"
    )
  )

  expect_equal(
    n21,
    c(
      `00` = "Night",
      `01` = "Night",
      `02` = "Night",
      `03` = "Night",
      `04` = "Night",
      `05` = "Night",
      `06` = "Day",
      `07` = "Day",
      `08` = "Day",
      `09` = "Day",
      `10` = "Day",
      `11` = "Day",
      `12` = "Day",
      `13` = "Day",
      `14` = "Day",
      `15` = "Day",
      `16` = "Day",
      `17` = "Day",
      `18` = "Day",
      `19` = "Day",
      `20` = "Night",
      `21` = "Night",
      `22` = "Night",
      `23` = "Night"
    )
  )

})
