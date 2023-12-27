test_that("when", {
  td <- when()

  td_2 <- when(include_date = TRUE)

  td_3 <- when(type = 'time')

  td_4 <- when(type = 'time',
               start = "00:00:00",
               end = "23:59:59")

  td_5 <-
    when(
      type = 'time',
      values = c("00:00:00", "23:59:59", "00:00:00", "23:59:59")
    )

  td_6 <-
    when(values = c("2023-12-24", "2024-01-02", "2023-12-24", "2024-01-02"))

  expect_equal(when(start = "2023-12-24", end = "2023-12-24"),
               structure(
                 list(
                   type = "std",
                   locale = "Spanish_Spain.utf8",
                   start = structure(19715, class = "Date"),
                   end = structure(19715, class = "Date"),
                   values = NULL,
                   surrogate_key = TRUE,
                   week_starts_monday = TRUE,
                   levels = c("time", "day", "week", "month", "year"),
                   year_level = TRUE,
                   year_level_names = c("year", "decade"),
                   include_year = TRUE,
                   include_decade = FALSE,
                   week_level = TRUE,
                   week_level_names = c("year_week", "week", "week_date"),
                   include_week_date = FALSE,
                   include_week = TRUE,
                   include_year_week = TRUE,
                   month_level = TRUE,
                   month_level_names = c(
                     "year_month",
                     "month",
                     "month_name",
                     "month_num_name",
                     "month_abbr",
                     "month_num_abbr",
                     "year_quarter",
                     "quarter",
                     "year_semester",
                     "semester"
                   ),
                   include_month = TRUE,
                   include_year_month = TRUE,
                   include_month_name = TRUE,
                   include_month_abbr = FALSE,
                   include_month_num_name = TRUE,
                   include_month_num_abbr = FALSE,
                   include_quarter = FALSE,
                   include_year_quarter = FALSE,
                   include_semester = FALSE,
                   include_year_semester = FALSE,
                   day_level = TRUE,
                   day_level_names = c(
                     "date",
                     "year_day",
                     "quarter_day",
                     "month_day",
                     "week_day",
                     "day_name",
                     "day_num_name",
                     "day_abbr",
                     "day_num_abbr"
                   ),
                   include_month_day = TRUE,
                   include_week_day = TRUE,
                   include_day_name = TRUE,
                   include_day_abbr = FALSE,
                   include_day_num_name = TRUE,
                   include_day_num_abbr = FALSE,
                   include_quarter_day = FALSE,
                   include_year_day = FALSE,
                   include_date = TRUE,
                   time_level = FALSE,
                   time_level_names = c("time", "hour", "minute", "second",
                                        "day_part"),
                   include_time = TRUE,
                   include_hour = TRUE,
                   include_minute = TRUE,
                   include_second = TRUE,
                   include_day_part = TRUE,
                   day_part = c(
                     `00` = "Night",
                     `01` = "Night",
                     `02` = "Night",
                     `03` = "Night",
                     `04` = "Night",
                     `05` = "Morning",
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
                     `21` = "Night",
                     `22` = "Night",
                     `23` = "Night"
                   ),
                   table_name = NULL,
                   attribute_names = NULL,
                   table = NULL
                 ),
                 class = "when"
               ))

  expect_equal(td,
               td_2)

  expect_equal(
    c(
      td_3$time_level,
      td_3$year_level,
      td_3$week_level,
      td_3$month_level,
      td_3$day_level
    ),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )

  expect_equal(td_4,
               td_3)

  expect_equal(td_5$values,
               structure(
                 c(0, 86399),
                 class = c("hms", "difftime"),
                 units = "secs"
               ))

  expect_equal(td_6$values,
               structure(c(19715, 19724), class = "Date"))


})
