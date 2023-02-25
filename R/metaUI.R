#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @import shiny
#' @importFrom rlang := .data
#' @importFrom stats sd lm confint aggregate
#' @importFrom utils read.csv
NULL

# Declare . as global variable to remove warnings
utils::globalVariables(".")

# Dummy function to remove CMD CHECK warning for packages only used in code saved as character
# Since most of the code for the Shiny app is not actually saved as code in this package
# but as text to be customised, these packages are not recognised otherwise

no_check_warnings <- function() {
  return(TRUE)

  # Shiny app
  plotly::ggplotly()
  shinycssloaders::withSpinner()
  shinythemes::themeSelector()
  waffle::waffle()

  # Estimation packages
  metafor::rma.mv()
  meta::metabias()
  puniform::puniform()
  robumeta::robu()
  weightr::weightfunct()
  zcurve::zcurve()
}
