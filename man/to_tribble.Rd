% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_app.R
\name{to_tribble}
\alias{to_tribble}
\title{Convert a tibble/dataframe to tribble code}
\source{
Taken from timesaveR package, copyright also by Lukas Wallrich (2023)
}
\usage{
to_tribble(x, show = FALSE, digits = 5)
}
\arguments{
\item{x}{The tibble/dataframe to be converted into tribble code}

\item{show}{Logical. Print code (otherwise, returned - print with \code{cat()} to get linebreaks etc)}

\item{digits}{Number of digits to round numeric columns to.}
}
\value{
Code to create a tibble using the \code{tribble()}-function
}
\description{
Tribbles are an easy way to legibly input data, and therefore helpful for teaching
and interactive work. This function takes
a tibble and returns code that can recreate it. Note that this function converts
"NA" to NA and converts factors to characters to retain the levels.
}
\examples{
metaUI:::to_tribble(mtcars, show = TRUE)
}
\keyword{internal}
