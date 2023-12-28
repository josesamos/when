test_that("level_year", {
  td_1 <- when() |>
    include_year_level(year = FALSE)

  td_2 <- td_1 |>
    include_year_level(include_all = TRUE)

  td_3 <- td_1 |>
    include_year_level()

  expect_equal(
    td_1$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'year'])],
    c(year = FALSE, decade = FALSE)
  )

  expect_equal(
    td_2$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'year'])],
    c(year = TRUE, decade = TRUE)
  )

  expect_equal(
    td_3$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'year'])],
    c(year = TRUE, decade = FALSE)
  )

})
