test_that("levels_date", {
  td_1 <- when() |>
    select_date_levels(day_level = FALSE)
  td_2 <- td_1 |>
    select_date_levels(include_all = TRUE, month_level = FALSE)

  td_3 <- td_1 |>
    select_date_levels(exclude_all = TRUE, month_level = TRUE)

  expect_equal(
    td_1$level_include_conf,
    c(
      time = FALSE,
      day = FALSE,
      week = TRUE,
      month = TRUE,
      quarter = FALSE,
      semester = FALSE,
      year = TRUE
    )
  )

  expect_equal(
    td_2$level_include_conf,
    c(
      time = FALSE,
      day = TRUE,
      week = TRUE,
      month = FALSE,
      quarter = TRUE,
      semester = TRUE,
      year = TRUE
    )
  )

  expect_equal(
    td_3$level_include_conf,
    c(
      time = FALSE,
      day = FALSE,
      week = FALSE,
      month = TRUE,
      quarter = FALSE,
      semester = FALSE,
      year = FALSE
    )
  )

})
