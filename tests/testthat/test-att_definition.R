test_that("att_definition", {
  wd <- when()

  f <- wd |>
    get_attribute_definition_function(name = "year")

  g <- function(table, values, ...) {
    table[['year']] <- '2025'
    table
  }

  wd <- wd |>
    set_attribute_definition_function(name = "year", g)

  h <- wd |>
    get_attribute_definition_function(name = "year")

  t <- wd |>
    generate_table() |>
    get_table()


  expect_equal(g, h)

  expect_equal(t[['year']], "2025")

})
