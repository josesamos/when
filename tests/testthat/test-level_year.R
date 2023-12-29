test_that("level_year", {
  td_1 <- when() |>
    select_year_level(year = FALSE)

  td_2 <- td_1 |>
    select_year_level(include_all = TRUE)

  td_3 <- td_1 |>
    select_year_level(include_all = TRUE, decade = FALSE)

  td_4 <- when() |>
    select_year_level(exclude_all = TRUE, year = TRUE)

  td_5 <- when() |>
    select_year_level(exclude_all = TRUE)

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

  expect_equal(
    td_4$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'year'])],
    c(year = TRUE, decade = FALSE)
  )

  expect_equal(
    td_5$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'year'])],
    c(year = FALSE, decade = FALSE)
  )

})
