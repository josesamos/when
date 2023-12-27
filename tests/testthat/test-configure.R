test_that("configure", {
  td_1 <- when()
  td_1$table_name <- NULL
  td_1 <- td_1 |>
    configure_dimension(name = 'when', type = 'time')

  td_2 <- when() |>
    configure_dimension(type = 'time')

  td_3 <- when() |>
    configure_dimension()

  wt <- when(type = 'time')
  wt_2 <- when() |>
    configure_dimension(type = 'time')

  expect_equal(
    c(
      td_1$day_level,
      td_1$week_level,
      td_1$month_level,
      td_1$year_level,
      td_1$time_level
    ),
    c(FALSE, FALSE, FALSE, FALSE, TRUE)
  )

  expect_equal(c(td_1$type, td_1$name),
               c(td_2$type, td_2$name))

  expect_equal(td_3$type,
               "std")

  expect_equal(td_3$time_level,
               FALSE)

  expect_equal(wt, wt_2)

})
