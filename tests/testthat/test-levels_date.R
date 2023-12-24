test_that("levels_date", {
  td_1 <- when() |>
    include_date_levels(day_level = FALSE)
  td_2 <- td_1 |>
    include_date_levels()

  expect_equal(
    c(
      td_1$day_level,
      td_1$week_level,
      td_1$month_level,
      td_1$year_level
    ),
    c(FALSE, TRUE, TRUE, TRUE)
  )

  expect_equal(
    c(
      td_2$day_level,
      td_2$week_level,
      td_2$month_level,
      td_2$year_level
    ),
    c(TRUE, TRUE, TRUE, TRUE)
  )

})
