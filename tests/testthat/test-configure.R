test_that("configure", {
  td_1 <- when()
  td_1 <- td_1 |>
    configure_dimension(name = 'when', type = 'time')

  td_2 <- when() |>
    configure_dimension(type = 'time')

  td_3 <- when() |>
    configure_dimension()

  td_4 <- when() |>
    configure_dimension(surrogate_key = FALSE)

  td_5 <- when(surrogate_key = FALSE)

  td_6 <- td_2 |>
    configure_dimension()

  td_7 <- td_4 |>
    configure_dimension()

  wt <- when(type = 'time')
  wt_2 <- when() |>
    configure_dimension(type = 'time')

  expect_equal(
    td_1$level_include_conf,
    c(
      time = TRUE,
      day = FALSE,
      week = FALSE,
      month = FALSE,
      year = FALSE
    )
  )

  expect_equal(c(td_1$type, td_1$name),
               c(td_2$type, td_2$name))

  expect_equal(td_3$type,
               "date")

  expect_equal(
    td_3$level_include_conf,
    c(
      time = FALSE,
      day = TRUE,
      week = TRUE,
      month = TRUE,
      year = TRUE
    )
  )

  expect_equal(wt, wt_2)

  expect_equal(td_3$surrogate_key, TRUE)

  expect_equal(td_4$surrogate_key, FALSE)

  expect_equal(td_5$surrogate_key, FALSE)

  expect_equal(td_2, td_6)

  expect_equal(td_4, td_7)

})

