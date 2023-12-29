test_that("configure", {
  td_1 <- when()
  td_1 <- td_1 |>
    define_characteristics(name = 'when', type = 'time')

  td_2 <- when() |>
    define_characteristics(type = 'time')

  td_3 <- when() |>
    define_characteristics()

  td_4 <- when() |>
    define_characteristics(surrogate_key = FALSE)

  td_5 <- when(surrogate_key = FALSE)

  td_6 <- td_2 |>
    define_characteristics()

  td_7 <- td_4 |>
    define_characteristics()

  wt <- when(type = 'time')
  wt_2 <- when() |>
    define_characteristics(type = 'time')

  expect_equal(
    td_1$level_include_conf,
    c(
      time = TRUE,
      day = FALSE,
      week = FALSE,
      month = FALSE,
      quarter = FALSE,
      semester = FALSE,
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
      quarter = FALSE,
      semester = FALSE,
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
