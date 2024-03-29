% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/levels_date.R
\name{select_day_level}
\alias{select_day_level}
\alias{select_day_level.when}
\title{Configure day level}
\usage{
select_day_level(
  td,
  include_all,
  exclude_all,
  date,
  month_day,
  week_day,
  day_name,
  day_num_name,
  day_abbr,
  day_num_abbr,
  quarter_day,
  year_day
)

\method{select_day_level}{when}(
  td,
  include_all = FALSE,
  exclude_all = FALSE,
  date = NULL,
  month_day = NULL,
  week_day = NULL,
  day_name = NULL,
  day_num_name = NULL,
  day_abbr = NULL,
  day_num_abbr = NULL,
  quarter_day = NULL,
  year_day = NULL
)
}
\arguments{
\item{td}{A \code{when} object.}

\item{include_all}{A boolean, include all fields of the level.}

\item{exclude_all}{A boolean, exclude all fields of the level.}

\item{date}{A boolean, include the date.}

\item{month_day}{A boolean, include the day number in the month.}

\item{week_day}{A boolean, the day number in the week.}

\item{day_name}{A boolean, include the name of the day of the week.}

\item{day_num_name}{A boolean, include the number and name of the day
of the week.}

\item{day_abbr}{A boolean, include the name of the day of the week in
abbreviated version.}

\item{day_num_abbr}{A boolean, include the number and name of the day
of the week in abbreviated version.}

\item{quarter_day}{A boolean, include the number of the day in the quarter.}

\item{year_day}{A boolean, include the number of the day in the year.}
}
\value{
A \code{when} object.
}
\description{
When the dimension is defined as a date type, using this function we can select
the day level and its attributes to include in it: date, month_day, week_day,
quarter_day and year_day.
}
\details{
The \code{include_all} and \code{exclude_all} parameters allow us to include or exclude
all attributes, and then specifically exclude or include the ones we need.

For the week_day we have the day number, its name and the name abbreviation.
So that the order of the names corresponds to the alphabetical order, the
combination of day number and name and/or abbreviation is included.
}
\examples{

td <- when() |>
  select_day_level(day_abbr = FALSE,
                    day_num_abbr = FALSE)

}
\seealso{
Other dimension definition: 
\code{\link{define_characteristics}()},
\code{\link{define_instances}()},
\code{\link{get_attribute_definition_function}()},
\code{\link{get_day_part}()},
\code{\link{get_week_date_range}()},
\code{\link{select_date_levels}()},
\code{\link{select_month_level}()},
\code{\link{select_quarter_level}()},
\code{\link{select_semester_level}()},
\code{\link{select_time_level}()},
\code{\link{select_week_level}()},
\code{\link{select_year_level}()},
\code{\link{set_attribute_definition_function}()},
\code{\link{set_day_part}()},
\code{\link{when}()}
}
\concept{dimension definition}
