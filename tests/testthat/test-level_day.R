test_that("level_day", {
  td_1 <- when() |>
    configure_day_level(day_abbr = TRUE,
                      day_num_name = FALSE)
  td_2 <- td_1 |>
    configure_day_level(include_all = TRUE)

  td_3 <- td_1 |>
    configure_day_level()

  expect_equal(
    td_1$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'day'])],
    c(
      date = TRUE,
      month_day = TRUE,
      week_day = TRUE,
      day_name = TRUE,
      day_num_name = FALSE,
      day_abbr = TRUE,
      day_num_abbr = FALSE,
      year_day = FALSE,
      quarter_day = FALSE
    )
  )

  expect_equal(
    td_2$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'day'])],
    c(
      date = TRUE,
      month_day = TRUE,
      week_day = TRUE,
      day_name = TRUE,
      day_num_name = TRUE,
      day_abbr = TRUE,
      day_num_abbr = TRUE,
      year_day = TRUE,
      quarter_day = TRUE
    )
  )

  expect_equal(
    td_3$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'day'])],
    c(
      date = TRUE,
      month_day = TRUE,
      week_day = TRUE,
      day_name = TRUE,
      day_num_name = FALSE,
      day_abbr = TRUE,
      day_num_abbr = FALSE,
      year_day = FALSE,
      quarter_day = FALSE
    )
  )

})
