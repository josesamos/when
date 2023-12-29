test_that("level_quarter", {
  td_1 <- when() |>
    select_quarter_level(quarter = FALSE)
  td_2 <- td_1 |>
    select_quarter_level(include_all = TRUE)

  expect_equal(td_1$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'quarter'])],
               c(year_quarter = TRUE, quarter = FALSE))

  expect_equal(td_2$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'quarter'])],
               c(year_quarter = TRUE, quarter = TRUE))

})
