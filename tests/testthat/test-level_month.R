test_that("level_month", {
  td_1 <- when() |>
    configure_month_level(month_name = FALSE,
                        month_num_name = FALSE)
  td_2 <- td_1 |>
    configure_month_level(include_all = TRUE)

  td_3 <- td_1 |>
    configure_month_level()

  expect_equal(
    td_1$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'month'])],
    c(
      year_month = TRUE,
      month = TRUE,
      month_name = FALSE,
      month_num_name = FALSE,
      month_abbr = FALSE,
      month_num_abbr = FALSE
    )
  )

  expect_equal(
    td_2$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'month'])],
    c(
      year_month = TRUE,
      month = TRUE,
      month_name = TRUE,
      month_num_name = TRUE,
      month_abbr = TRUE,
      month_num_abbr = TRUE
    )
  )

  expect_equal(
    td_3$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'month'])],
    c(
      year_month = TRUE,
      month = TRUE,
      month_name = FALSE,
      month_num_name = FALSE,
      month_abbr = FALSE,
      month_num_abbr = FALSE
    )
  )

})
