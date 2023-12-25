test_that("configure", {
  td_1 <- when() |>
    configure_dimension(name = 'when', type = 'time')

  td_2 <- when() |>
    configure_dimension(type = 'time')

  td_3 <- when() |>
    configure_dimension()

  expect_equal(td_1,
               td_2)

  expect_equal(td_3$type,
               "std")

})
