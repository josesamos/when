test_that("instances", {
  td_1 <- when() |>
    define_instances(start = "2020", end = "2030")

  td_2 <- when() |>
    define_instances(values = 2020:2030)


  td_3 <- when(type = 'time') |>
    define_instances(start = 1, end = 5)

  td_4 <- when(type = 'time') |>
    define_instances(values = 1:5)

  expect_equal(c(td_1$start,
                 td_1$end),
               structure(c(18262, 21915), class = "Date"))


  expect_equal(c(td_2$values),
               structure(
                 c(
                   18262,
                   18628,
                   18993,
                   19358,
                   19723,
                   20089,
                   20454,
                   20819,
                   21184,
                   21550,
                   21915
                 ),
                 class = "Date"
               ))

  expect_equal(c(td_3$start,
                 td_3$end),
               structure(
                 c(3600, 18000),
                 units = "secs",
                 class = c("hms", "difftime")
               ))


  expect_equal(c(td_4$values),
               structure(
                 c(3600, 7200, 10800, 14400, 18000),
                 units = "secs",
                 class = c("hms",
                           "difftime")
               ))


})
