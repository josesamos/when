test_that("level_time", {
  td_1 <- when() |>
    configure_time_level(minute = FALSE)
  td_2 <- td_1 |>
    configure_time_level()

  td_5 <- td_1 |>
    configure_time_level(include_all = TRUE)

  td_4 <- td_1 |>
    configure_time_level(exclude_all = TRUE)

  td_6 <- td_1 |>
    configure_time_level(include_all = TRUE, minute = FALSE)

  td_3 <- td_1 |>
    set_day_part(hour = c(21:23, 0:5), name = "Noche")

  expect_equal(
    td_1$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'time'])],
    c(time = TRUE, hour = TRUE, minute = FALSE, second = FALSE, day_part = TRUE
    )
  )

  expect_equal(
    td_2$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'time'])],
    c(time = TRUE, hour = TRUE, minute = FALSE, second = FALSE, day_part = TRUE
    )
  )

  expect_equal(
    td_5$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'time'])],
    c(time = TRUE, hour = TRUE, minute = TRUE, second = TRUE, day_part = TRUE
    )
  )

  expect_equal(
    td_4$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'time'])],
    c(time = FALSE, hour = TRUE, minute = FALSE, second = FALSE, day_part = FALSE
    )
  )

  expect_equal(
    td_6$att_include_conf[names(td_1$att_levels[td_1$att_levels == 'time'])],
    c(time = TRUE, hour = TRUE, minute = FALSE, second = FALSE, day_part = TRUE
    )
  )

  expect_equal(
    td_3$day_part,
    c(
      `00` = "Noche",
      `01` = "Noche",
      `02` = "Noche",
      `03` = "Noche",
      `04` = "Noche",
      `05` = "Noche",
      `06` = "Morning",
      `07` = "Morning",
      `08` = "Morning",
      `09` = "Morning",
      `10` = "Morning",
      `11` = "Morning",
      `12` = "Afternoon",
      `13` = "Afternoon",
      `14` = "Afternoon",
      `15` = "Afternoon",
      `16` = "Afternoon",
      `17` = "Evening",
      `18` = "Evening",
      `19` = "Evening",
      `20` = "Evening",
      `21` = "Noche",
      `22` = "Noche",
      `23` = "Noche"
    )
  )


  expect_equal(
    td_3 |> get_day_part(),
    td_3$day_part
  )

})
