test_that("level_week", {
  td_1 <- when() |>
    include_week_level(week = FALSE)
  td_2 <- td_1 |>
    include_week_level(include_all = TRUE)
  td_3 <- td_1 |>
    include_week_level()

  expect_equal(
    td_1$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'week'])],
    c(year_week = TRUE, week = FALSE, week_date = FALSE)
  )

  expect_equal(
    td_2$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'week'])],
    c(year_week = TRUE, week = TRUE, week_date = TRUE)
  )

  expect_equal(
    td_3$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'week'])],
    c(year_week = TRUE, week = TRUE, week_date = FALSE)
  )

})
