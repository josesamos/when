test_that("level_year", {
  td_1 <- when() |>
    include_year_level(include_decade = FALSE)
  td_2 <- td_1 |>
    include_year_level()

  expect_equal(
    c(
      td_1$year_level,
      td_1$include_year,
      td_1$include_decade
    ),
    c(TRUE, TRUE, FALSE)
  )

  expect_equal(
    c(
      td_2$year_level,
      td_2$include_year,
      td_2$include_decade
    ),
    c(TRUE, TRUE, TRUE)
  )

})
