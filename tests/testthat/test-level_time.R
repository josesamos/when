test_that("level_time", {
  td_1 <- when() |>
    include_week_level(include_week_date = FALSE)
  td_2 <- td_1 |>
    include_week_level()

  expect_equal(
    c(
      td_1$week_level,
      td_1$include_week,
      td_1$include_year_week,
      td_1$include_week_date
    ),
    c(TRUE, TRUE, TRUE, FALSE)
  )

  expect_equal(
    c(
      td_2$week_level,
      td_2$include_week,
      td_2$include_year_week,
      td_2$include_week_date
    ),
    c(TRUE, TRUE, TRUE, TRUE)
  )

})
