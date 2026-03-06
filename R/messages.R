#' Less scary green messages
#'
#' @param ... message components (see [base::message()])
#' @param domain (see [base::message()])
#' @param appendLF append new line? (see [base::message()])
#'
#' @return TRUE
#' @keywords internal
#'
message <- function (..., domain = NULL, appendLF = TRUE) {
  if (is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
    # not in knitr environment
    base::message("\033[32m", ..., "\033[39m", 
                  domain = domain, appendLF = appendLF)
  } else {
    base::message(..., domain = domain, appendLF = appendLF)
  }
}
