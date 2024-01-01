test_that("examples", {

  tt1 <- when(type = 'time', start = "01:01:01", end = "23:23:23") |>
    generate_table() |>
    get_table()

  tt2 <- when(type = 'time', start = "01:01:01", end = "23:23:23", second = FALSE) |>
    generate_table() |>
    get_table()

  tt3 <- when(type = 'time', start = "01:01:01", end = "23:23:23", minute = FALSE) |>
    generate_table() |>
    get_table()

  td1 <- when(start = "2024-01-01", end = "2029-12-31") |>
    generate_table() |>
    get_table()


  expect_equal(nrow(tt1), 80543)

  expect_equal(
    names(tt1),
    c("id", "time", "hour", "minute", "second", "day_part")
  )

  expect_equal(nrow(tt2), 1343)

  expect_equal(
    names(tt2),
    c("id", "time", "hour", "minute", "day_part")
  )

  expect_equal(nrow(tt3), 23)

  expect_equal(
    names(tt3),
    c("id", "time", "hour", "day_part")
  )


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

})
