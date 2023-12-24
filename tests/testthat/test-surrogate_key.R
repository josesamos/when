test_that("surrogate_key", {
  td <- when() |>
    include_surrogate_key()

  td_2 <- when() |>
    include_surrogate_key(FALSE)

  expect_equal(td$surrogate_key,
               TRUE)

  expect_equal(td_2$surrogate_key,
               FALSE)

})
