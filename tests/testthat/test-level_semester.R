test_that("level_semester", {
  td_1 <- when() |>
    configure_semester_level(semester = FALSE)
  td_2 <- td_1 |>
    configure_semester_level(include_all = TRUE)

  expect_equal(td_1$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'semester'])],
               c(year_semester = TRUE, semester = FALSE))

  expect_equal(td_2$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'semester'])],
               c(year_semester = TRUE, semester = TRUE))

})
