test_that("levels_date", {
  td_1 <- when() |>
    include_date_levels(day_level = FALSE)
  td_2 <- td_1 |>
    include_date_levels()

  expect_equal(
    td_1$level_include_conf,
    c(
      time = FALSE,
      day = FALSE,
      week = TRUE,
      month = TRUE,
      year = TRUE
    )
  )

  expect_equal(
    td_2$level_include_conf,
    c(
      time = FALSE,
      day = TRUE,
      week = TRUE,
      month = TRUE,
      year = TRUE
    )
  )

})
