test_that("level_year", {
  td_1 <- when() |>
    include_year_level(include_year = FALSE)

  td_2 <- td_1 |>
    include_year_level(include_all = TRUE)

  td_3 <- td_1 |>
    include_year_level()

  expect_equal(
    c(
      td_1$year_level,
      td_1$include_year,
      td_1$include_decade
    ),
    c(TRUE, FALSE, FALSE)
  )

  expect_equal(
    c(
      td_2$year_level,
      td_2$include_year,
      td_2$include_decade
    ),
    c(TRUE, TRUE, TRUE)
  )

  expect_equal(
    c(
      td_3$year_level,
      td_3$include_year,
      td_3$include_decade
    ),
    c(TRUE, TRUE, FALSE)
  )

})
