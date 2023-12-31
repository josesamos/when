test_that("when", {
  td <- when()

  td_2 <- when(date = TRUE)

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

  expect_equal({
    d <- when(start = "2023-12-24", end = "2023-12-24")
    d$att_function <- NULL
    d$locale <- NULL
    d
  },
  structure(
    list(
      type = "date",
      start = structure(19715, class = "Date"),
      end = structure(19715, class = "Date"),
      values = NULL,
      surrogate_key = TRUE,
      week_starts_monday = TRUE,
      att_levels = c(
        time = "time",
        hour = "time",
        minute = "time",
        second = "time",
        day_part = "time",
        date = "day",
        month_day = "day",
        week_day = "day",
        day_name = "day",
        day_num_name = "day",
        day_abbr = "day",
        day_num_abbr = "day",
        year_day = "day",
        quarter_day = "day",
        year_week = "week",
        week = "week",
        week_date = "week",
        year_month = "month",
        month = "month",
        month_name = "month",
        month_num_name = "month",
        month_abbr = "month",
        month_num_abbr = "month",
        year_quarter = "quarter",
        quarter = "quarter",
        year_semester = "semester",
        semester = "semester",
        year = "year",
        decade = "year"
      ),
      level_type = c(
        time = "time",
        day = "date",
        week = "date",
        month = "date",
        quarter = "date",
        semester = "date",
        year = "date"
      ),
      att_include_conf = c(
        time = TRUE,
        hour = TRUE,
        minute = TRUE,
        second = TRUE,
        day_part = TRUE,
        date = TRUE,
        month_day = TRUE,
        week_day = TRUE,
        day_name = TRUE,
        day_num_name = TRUE,
        day_abbr = FALSE,
        day_num_abbr = FALSE,
        year_day = FALSE,
        quarter_day = FALSE,
        year_week = TRUE,
        week = TRUE,
        week_date = FALSE,
        year_month = TRUE,
        month = TRUE,
        month_name = TRUE,
        month_num_name = TRUE,
        month_abbr = FALSE,
        month_num_abbr = FALSE,
        year_quarter = TRUE,
        quarter = TRUE,
        year_semester = TRUE,
        semester = TRUE,
        year = TRUE,
        decade = FALSE
      ),
      level_include_conf = c(
        time = FALSE,
        day = TRUE,
        week = TRUE,
        month = TRUE,
        quarter = FALSE,
        semester = FALSE,
        year = TRUE
      ),
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
    td_3$level_include_conf,
    c(
      time = TRUE,
      day = FALSE,
      week = FALSE,
      month = FALSE,
      quarter = FALSE,
      semester = FALSE,
      year = FALSE
    )
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
