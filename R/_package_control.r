
# edit the DESCRIPTION file using these commands
# library(usethis)


# This block will update the DESCRIPTION file when devtools::document() is run (or via shift-ctrl-d) ####
usethis::use_package("dplyr")
usethis::use_package("lubridate")
usethis::use_package("magrittr")
usethis::use_package("precis")
usethis::use_package("scales")
usethis::use_package("stats")
usethis::use_package("tibble")
usethis::use_package("zoo")


# This block will update the NAMESPACE file when devtools::document() is run (or via shift-ctrl-d) ####

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
