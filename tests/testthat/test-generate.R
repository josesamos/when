test_that("generate", {
  td_1 <- when(type = 'time') |>
    generate_table()

  td_2 <- when() |>
    generate_table()

  td_3 <- when() |>
    include_day_level() |>
    include_week_level() |>
    include_month_level() |>
    include_year_level() |>
    generate_table()

  td_4 <- when(type = 'time', include_minute = FALSE) |>
    generate_table()

  td_5 <- when(day_level = FALSE) |>
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
      include_second = FALSE
    ) |>
    generate_table()

  td_8 <-
    when(
      type = 'time',
      start = 1,
      end = 3,
      include_minute = FALSE
    ) |>
    generate_table()

  td_9 <- when(start = 2023, end = 2024) |>
    generate_table()

  td_10 <- when(start = 2023,
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

  td_13 <- when(values = 2023:2024, day_level = FALSE, week_level = FALSE, month_level = FALSE) |>
    generate_table()

  td_14 <- when(values = 2023:2024, day_level = FALSE, week_level = FALSE) |>
    generate_table()

  td_15 <- when(values = 2023:2024, day_level = FALSE) |>
    generate_table()

  td_16 <- when(values = 2023:2024) |>
    generate_table()

  expect_equal(nrow(td_1$table),
               86400)

  expect_equal(names(td_1$table),
               c("id", "time", "hour", "minute", "second", "day_part"))

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
      "year_day",
      "quarter_day",
      "month_day",
      "week_day",
      "day_name",
      "day_num_name",
      "day_abbr",
      "day_num_abbr",
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

})
