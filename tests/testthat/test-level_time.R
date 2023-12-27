test_that("level_time", {
  td_1 <- when() |>
    include_time_level(minute = FALSE)
  td_2 <- td_1 |>
    include_time_level()

  td_3 <- td_1 |>
    set_day_part(hour = c(21:23, 0:5), name = "Noche")

  expect_equal(
    c(
      td_1$include_time,
      td_1$include_minute,
      td_1$include_second,
      td_1$include_day_part
    ),
    c(TRUE, FALSE, FALSE, TRUE)
  )

  expect_equal(
    c(
      td_2$include_time,
      td_2$include_minute,
      td_2$include_second,
      td_2$include_day_part
    ),
    c(TRUE, TRUE, TRUE, TRUE)
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
