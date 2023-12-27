test_that("level_day", {
  td_1 <- when() |>
    include_day_level(day_abbr = TRUE,
                      day_num_abbr = FALSE)
  td_2 <- td_1 |>
    include_day_level(include_all = TRUE)

  td_3 <- td_1 |>
    include_day_level()

  expect_equal(
    c(
      td_1$day_level,
      td_1$include_date,
      td_1$include_month_day,
      td_1$include_week_day,
      td_1$include_day_name,
      td_1$include_day_abbr,
      td_1$include_day_num_name,
      td_1$include_day_num_abbr,
      td_1$include_quarter_day,
      td_1$include_year_day
    ),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  expect_equal(
    c(
      td_2$day_level,
      td_2$include_date,
      td_2$include_month_day,
      td_2$include_week_day,
      td_2$include_day_name,
      td_2$include_day_abbr,
      td_2$include_day_num_name,
      td_2$include_day_num_abbr,
      td_2$include_quarter_day,
      td_2$include_year_day
    ),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )

  expect_equal(
    c(
      td_3$day_level,
      td_3$include_date,
      td_3$include_month_day,
      td_3$include_week_day,
      td_3$include_day_name,
      td_3$include_day_abbr,
      td_3$include_day_num_name,
      td_3$include_day_num_abbr,
      td_3$include_quarter_day,
      td_3$include_year_day
    ),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )

})
