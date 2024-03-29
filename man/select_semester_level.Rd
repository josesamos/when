% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/levels_date.R
\name{select_semester_level}
\alias{select_semester_level}
\alias{select_semester_level.when}
\title{Configure semester level}
\usage{
select_semester_level(td, include_all, exclude_all, semester, year_semester)

\method{select_semester_level}{when}(
  td,
  include_all = FALSE,
  exclude_all = FALSE,
  semester = NULL,
  year_semester = NULL
)
}
\arguments{
\item{td}{A \code{when} object.}

\item{include_all}{A boolean, include all fields of the level.}

\item{exclude_all}{A boolean, exclude all fields of the level.}

\item{semester}{A boolean, include the semester field.}

\item{year_semester}{A boolean, include the semester field.}
}
\value{
A \code{when} object.
}
\description{
When the dimension is defined as a date type, using this function we can select
the semester level and its attributes to include in it: semester number and the
combination of the year with it.
}
\details{
The \code{include_all} and \code{exclude_all} parameters allow us to include or exclude
all attributes, and then specifically exclude or include the ones we need.
}
\examples{

td <- when() |>
  select_semester_level(semester = FALSE)

}
\seealso{
Other dimension definition: 
\code{\link{define_characteristics}()},
\code{\link{define_instances}()},
\code{\link{get_attribute_definition_function}()},
\code{\link{get_day_part}()},
\code{\link{get_week_date_range}()},
\code{\link{select_date_levels}()},
\code{\link{select_day_level}()},
\code{\link{select_month_level}()},
\code{\link{select_quarter_level}()},
\code{\link{select_time_level}()},
\code{\link{select_week_level}()},
\code{\link{select_year_level}()},
\code{\link{set_attribute_definition_function}()},
\code{\link{set_day_part}()},
\code{\link{when}()}
}
\concept{dimension definition}
