test_that("level_month", {
  td_1 <- when() |>
    include_month_level(include_month_abbr = FALSE,
                        include_month_num_abbr = FALSE)
  td_2 <- td_1 |>
    include_month_level()

  expect_equal(
    c(
      td_1$month_level,
      td_1$include_month,
      td_1$include_year_month,
      td_1$include_month_name,
      td_1$include_month_abbr,
      td_1$include_month_num_name,
      td_1$include_month_num_abbr,
      td_1$include_quarter,
      td_1$include_year_quarter,
      td_1$include_semester,
      td_1$include_year_semester
    ),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )

  expect_equal(
    c(
      td_2$month_level,
      td_2$include_month,
      td_2$include_year_month,
      td_2$include_month_name,
      td_2$include_month_abbr,
      td_2$include_month_num_name,
      td_2$include_month_num_abbr,
      td_2$include_quarter,
      td_2$include_year_quarter,
      td_2$include_semester,
      td_2$include_year_semester
    ),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )

})
