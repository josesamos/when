
<!-- README.md is generated from README.Rmd. Please edit that file -->

# when <a href="https://josesamos.github.io/when/"><img src="man/figures/logo.png" align="right" height="139" alt="when website" /></a>

<!-- badges: start -->
<!-- badges: end -->

The *When* dimension plays a fundamental role in *Multidimensional
Systems*, it allows us to express **when** the analysed focus of
attention have occurred.

The purpose of the `when` package is to assist in the implementation of
the *When* dimension in multidimensional systems. In particular, it
supports the generation of tables for the implementation of **date** and
**time** in ROLAP (*Relational On-Line Analytical Processing*) systems.

## Installation

You can install the development version of `when` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("josesamos/when")
```

## Example

To obtain a table with dates we indicate the start date and the end
date. We can select the level of detail, the attributes to include or
the language of the literals for day and month names. In the following
example we only indicate the language because it is different from the
one we have in the computer’s operating system.

``` r
library(when)

date <-
  when(
    locale = Sys.setlocale("LC_TIME", "English"),
    start = lubridate::today(),
    end = lubridate::today() + 9
  ) |>
  generate_table() |>
  get_table()
```

The result obtained is shown below.

``` r
pander::pandoc.table(date, split.table = Inf)
```

| id  |    date    | month_day | week_day | day_name  | day_num_name | year_week | week | year_month | month | month_name | month_num_name | year |
|:---:|:----------:|:---------:|:--------:|:---------:|:------------:|:---------:|:----:|:----------:|:-----:|:----------:|:--------------:|:----:|
|  1  | 2023-12-23 |    23     |    6     | Saturday  |  6-Saturday  |  2023-51  |  51  |  2023-12   |  12   |  December  |  12-December   | 2023 |
|  2  | 2023-12-24 |    24     |    7     |  Sunday   |   7-Sunday   |  2023-52  |  52  |  2023-12   |  12   |  December  |  12-December   | 2023 |
|  3  | 2023-12-25 |    25     |    1     |  Monday   |   1-Monday   |  2023-52  |  52  |  2023-12   |  12   |  December  |  12-December   | 2023 |
|  4  | 2023-12-26 |    26     |    2     |  Tuesday  |  2-Tuesday   |  2023-52  |  52  |  2023-12   |  12   |  December  |  12-December   | 2023 |
|  5  | 2023-12-27 |    27     |    3     | Wednesday | 3-Wednesday  |  2023-52  |  52  |  2023-12   |  12   |  December  |  12-December   | 2023 |
|  6  | 2023-12-28 |    28     |    4     | Thursday  |  4-Thursday  |  2023-52  |  52  |  2023-12   |  12   |  December  |  12-December   | 2023 |
|  7  | 2023-12-29 |    29     |    5     |  Friday   |   5-Friday   |  2023-52  |  52  |  2023-12   |  12   |  December  |  12-December   | 2023 |
|  8  | 2023-12-30 |    30     |    6     | Saturday  |  6-Saturday  |  2023-52  |  52  |  2023-12   |  12   |  December  |  12-December   | 2023 |
|  9  | 2023-12-31 |    31     |    7     |  Sunday   |   7-Sunday   |  2023-53  |  53  |  2023-12   |  12   |  December  |  12-December   | 2023 |
| 10  | 2024-01-01 |    01     |    1     |  Monday   |   1-Monday   |  2024-01  |  01  |  2024-01   |  01   |  January   |   01-January   | 2024 |

If we want a table with time, we indicate the type using a parameter. By
default we get all the seconds of a day but we can also configure a
different period or level of detail.

``` r
time <-
  when(type = 'time') |>
  generate_table() |>
  get_table()
```

The start and end of the result table is shown below.

``` r
pander::pandoc.table(rbind(head(time, 5), tail(time, 5)),
                     split.table = Inf)
```

|  id   |   time   | hour | minute | second |
|:-----:|:--------:|:----:|:------:|:------:|
|   1   | 00:00:00 |  00  |   00   |   00   |
|   2   | 00:00:01 |  00  |   00   |   01   |
|   3   | 00:00:02 |  00  |   00   |   02   |
|   4   | 00:00:03 |  00  |   00   |   03   |
|   5   | 00:00:04 |  00  |   00   |   04   |
| 86396 | 23:59:55 |  23  |   59   |   55   |
| 86397 | 23:59:56 |  23  |   59   |   56   |
| 86398 | 23:59:57 |  23  |   59   |   57   |
| 86399 | 23:59:58 |  23  |   59   |   58   |
| 86400 | 23:59:59 |  23  |   59   |   59   |

In addition to obtaining them as `tibble`, we can export the tables to
files in *csv* or *xlsx* format. They can also be exported directly to
any Relational DBMS.
