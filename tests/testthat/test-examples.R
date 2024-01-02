test_that("examples", {
  tt1 <- when(type = 'time',
              start = "01:01:01",
              end = "23:23:23") |>
    generate_table() |>
    get_table()

  tt2 <-
    when(
      type = 'time',
      start = "01:01:01",
      end = "23:23:23",
      second = FALSE
    ) |>
    generate_table() |>
    get_table()

  tt3 <-
    when(
      type = 'time',
      start = "01:01:01",
      end = "23:23:23",
      minute = FALSE
    ) |>
    generate_table() |>
    get_table()

  td1 <- when(start = "2024-01-01", end = "2029-12-31") |>
    generate_table() |>
    get_table()

  td2 <-
    when(start = "2024-01-01",
         end = "2029-12-31",
         day_level = FALSE) |>
    generate_table() |>
    get_table()

  td3 <-
    when(
      start = "2024-01-01",
      end = "2029-12-31",
      day_level = FALSE,
      month_level = FALSE
    ) |>
    generate_table() |>
    get_table()

  td4 <-
    when(
      start = "2024-01-01",
      end = "2029-12-31",
      day_level = FALSE,
      week_level = FALSE
    ) |>
    generate_table() |>
    get_table()

  td5 <-
    when(
      start = "2024-01-01",
      end = "2029-12-31",
      day_level = FALSE,
      week_level = FALSE,
      month_level = FALSE
    ) |>
    generate_table() |>
    get_table()

  td6 <-
    when(
      start = "2024-01-01",
      end = "2029-12-31",
      day_level = FALSE,
      week_level = FALSE,
      month_level = FALSE,
      quarter_level = TRUE
    ) |>
    generate_table() |>
    get_table()

  td7 <-
    when(
      start = "2024-01-01",
      end = "2029-12-31",
      day_level = FALSE,
      month_level = FALSE,
      quarter_level = TRUE
    ) |>
    generate_table() |>
    get_table()


  expect_equal(nrow(tt1), 80543)

  expect_equal(names(tt1),
               c("id", "time", "hour", "minute", "second", "day_part"))

  expect_equal(nrow(tt2), 1343)

  expect_equal(names(tt2),
               c("id", "time", "hour", "minute", "day_part"))

  expect_equal(nrow(tt3), 23)

  expect_equal(names(tt3),
               c("id", "time", "hour", "day_part"))

  expect_equal(nrow(td1), 2192)

  expect_equal(
    names(td1),
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

  expect_equal(nrow(td2), 376)

  expect_equal(
    names(td2),
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

  expect_equal(nrow(td3), 318)

  expect_equal(names(td3),
               c("id", "year_week", "week", "year"))

  expect_equal(nrow(td4), 72)

  expect_equal(names(td4),
               c(
                 "id",
                 "year_month",
                 "month",
                 "month_name",
                 "month_num_name",
                 "year"
               ))

  expect_equal(nrow(td5), 6)

  expect_equal(names(td5),
               c("id", "year"))

  expect_equal(nrow(td6), 24)

  expect_equal(names(td6),
               c("id", "year_quarter", "quarter", "year"))

  expect_equal(nrow(td7), 328)

  expect_equal(names(td7),
               c("id", "year_week", "week", "year_quarter", "quarter", "year"))

})
