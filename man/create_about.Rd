% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_app.R
\name{create_about}
\alias{create_about}
\title{Create the about text for the app}
\usage{
create_about(
  dataset_name,
  date = format(Sys.Date(), "\%d \%b \%Y"),
  citation = "",
  osf_link = "",
  contact = "",
  list_packages = TRUE
)
}
\arguments{
\item{dataset_name}{Name of the dataset}

\item{date}{Date of the last update. Defaults to the current date.}

\item{citation}{Citation requested by the author. Defaults to an empty string.}

\item{osf_link}{Link to the OSF (or similar) project where data and materials are available. Defaults to an empty string.}

\item{contact}{Contact information, typically email. Defaults to an empty string.}

\item{list_packages}{Should all packages used by the app be listed on the About page? If TRUE, it list the packages
used in the default configuration. To display different packages (e.g., because you added/removed models), pass
a character vector with all packages to display.}
}
\value{
HTML code for the \code{About} panel of the Shiny app
}
\description{
This function is generally only called internally by \code{\link[=generate_shiny]{generate_shiny()}}.
}
\keyword{internal}
