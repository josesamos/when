test_that("generate", {
  td <- when(type = 'time')

  td_1 <- when(type = 'time') |>
    generate_table()

  names0 <- td |>
    get_table_attribute_names()

  names <- td_1 |>
    get_table_attribute_names()

  td_1_1 <- td_1 |>
    set_table_attribute_names(c('id1', 'time1', 'hour1', 'minute1', 'second1', 'day_part1'))

  names1 <- td_1_1 |>
    get_table_attribute_names()

  td_2 <- when() |>
    generate_table()

  td_3 <- when() |>
    select_day_level(include_all = TRUE) |>
    select_week_level(include_all = TRUE) |>
    select_month_level(include_all = TRUE) |>
    select_year_level(include_all = TRUE) |>
    generate_table()

  td_4 <- when(type = 'time', minute = FALSE) |>
    generate_table()

  td_5 <- when(type = 'iso', day_level = FALSE) |>
    generate_table()

  td_6 <- when(type = 'time',
               start = 1,
               end = 3) |>
    generate_table()

  td_7 <-
    when(
      type = 'time',
      start = 1,
      end = 3,
      second = FALSE
    ) |>
    generate_table()

  td_8 <-
    when(
      type = 'time',
      start = 1,
      end = 3,
      minute = FALSE
    ) |>
    generate_table()

  td_9 <- when(start = 2023, end = 2024) |>
    generate_table()

  td_10 <- when(type = 'iso',
                start = 2023,
                end = 2024,
                day_level = FALSE) |>
    generate_table()

  td_11 <-
    when(
      start = 2023,
      end = 2024,
      day_level = FALSE,
      week_level = FALSE
    ) |>
    generate_table()

  td_12 <-
    when(
      start = 2023,
      end = 2024,
      day_level = FALSE,
      week_level = FALSE,
      month_level = FALSE
    ) |>
    generate_table()

  td_13 <-
    when(
      values = 2023:2024,
      day_level = FALSE,
      week_level = FALSE,
      month_level = FALSE
    ) |>
    generate_table()

  td_14 <-
    when(values = 2023:2024,
         day_level = FALSE,
         week_level = FALSE) |>
    generate_table()

  td_15 <- when(type = 'iso', values = 2023:2024, day_level = FALSE) |>
    generate_table()

  td_16 <- when(values = 2023:2024) |>
    generate_table()

  td_17 <- when(type = 'time', minute = FALSE)
  td_18 <- when(week_level = FALSE, date = FALSE)
  ln_17 <- td_17 |>
    get_level_names(selected = TRUE)

  ln_18 <- td_18 |>
    get_level_names()

  ln_18_s <- td_18 |>
    get_level_names(selected = TRUE)


  lan_17 <- td_17 |>
    get_level_attribute_names()

  lan_17_s <- td_17 |>
    get_level_attribute_names(selected = TRUE)

  lan_18 <- td_18 |>
    get_level_attribute_names()

  lan_18_s <- td_18 |>
    get_level_attribute_names(selected = TRUE)

  ln_1 <- td_1 |>
    get_level_names()

  ln_1_s <- td_1 |>
    get_level_names(selected = TRUE)

  ln_2 <- td_2 |>
    get_level_names()

  ln_2_s <- td_2 |>
    get_level_names(selected = TRUE)

  expect_equal(nrow(td_1$table),
               86400)

  expect_equal(names(td_1$table),
               c("id", "time", "hour", "minute", "second", "day_part"))

  expect_equal(names,
               "c('id', 'time', 'hour', 'minute', 'second', 'day_part')")

  expect_equal(names0,
               names)

  expect_equal(names1,
               "c('id1', 'time1', 'hour1', 'minute1', 'second1', 'day_part1')")

  expect_equal(nrow(td_2$table),
               1)

  expect_equal(
    names(td_2$table),
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
    names(td_3$table),
    c(
      "id",
      "date",
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "day_abbr",
      "day_num_abbr",
      "year_day",
      "quarter_day",
      "year_week",
      "week",
      "week_date",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "month_abbr",
      "month_num_abbr",
      "year",
      "decade"
    )
  )

  expect_equal(names(td_4$table),
               c("id", "time", "hour", "day_part"))

  expect_equal(
    names(td_5$table),
    c(
      "id",
      "year_week",
      "week",
      "week_date",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(nrow(td_6$table),
               7201)

  expect_equal(names(td_6$table),
               c("id", "time", "hour", "minute", "second", "day_part"))

  expect_equal(nrow(td_7$table),
               121)

  expect_equal(names(td_7$table),
               c("id", "time", "hour", "minute", "day_part"))

  expect_equal(nrow(td_8$table),
               3)

  expect_equal(names(td_8$table),
               c("id", "time", "hour", "day_part"))

  expect_equal(nrow(td_9$table),
               366)

  expect_equal(
    names(td_9$table),
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

  expect_equal(nrow(td_10$table),
               53)

  expect_equal(
    names(td_10$table),
    c(
      "id",
      "year_week",
      "week",
      "week_date",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(nrow(td_11$table),
               13)

  expect_equal(
    names(td_11$table),
    c(
      "id",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(nrow(td_12$table),
               2)

  expect_equal(names(td_12$table),
               c("id",
                 "year"))

  expect_equal(nrow(td_13$table),
               2)

  expect_equal(names(td_13$table),
               c("id",
                 "year"))

  expect_equal(nrow(td_14$table),
               2)

  expect_equal(
    names(td_11$table),
    c(
      "id",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(nrow(td_15$table),
               2)

  expect_equal(
    names(td_15$table),
    c(
      "id",
      "year_week",
      "week",
      "week_date",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

  expect_equal(nrow(td_16$table),
               2)

  expect_equal(
    names(td_16$table),
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

  expect_equal(ln_17,
               "time")

  expect_equal(ln_18,
               c("day", "week", "month", "quarter", "semester", "year"))

  expect_equal(ln_18_s,
               c("day", "month", "year"))

  expect_equal(lan_17,
               c("time", "hour", "minute", "second", "day_part"))

  expect_equal(lan_17_s,
               c("time", "hour", "day_part"))

  expect_equal(
    lan_18,
    c(
      "date",
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "day_abbr",
      "day_num_abbr",
      "year_day",
      "quarter_day",
      "year_week",
      "week",
      "week_date",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "month_abbr",
      "month_num_abbr",
      "year_quarter",
      "quarter",
      "year_semester",
      "semester",
      "year",
      "decade"
    )
  )

  expect_equal(
    lan_18_s,
    c(
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "year_month",
      "month",
      "month_name",
      "month_num_name",
      "year"
    )
  )

})
