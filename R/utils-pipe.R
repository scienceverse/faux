#' Pipe operator
#'
#' See `dplyr::[\%>\%][dplyr::\%>\%]` for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @returns The result of applying the function in the rhs to the value in the lhs.
NULL

#' Default value for `NULL`
#'
#' This infix function makes it easy to replace `NULL`s with a default value. It's inspired by the way that Ruby's or operation (`||`) works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @export
#' @keywords internal
#' @name op-null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Reexport from base on newer versions of R to avoid conflict messages
if (exists("%||%", envir = baseenv())) {
  `%||%` <- get("%||%", envir = baseenv())
}

#' Replace If
#'
#' Replace values if NULL, NA, or sepcified value
#'
#' @param x For each `x[[i]]` if NULL, return `y[[i]]`; otherwise return `x[[i]]`.
#' @param y Replacement value(s); if not the same length as `x`, then values will be recycled
#' @export
#' @keywords internal
#' @name op-vect-null-default
#' @examples
#' list(NULL, 1) %|||% 2
#' list(NULL, 0, NULL) %|||% 1:3
rep_if <- function(x, y, replace = NULL) {
  y <- rep_len(y, length(x))
  for (i in seq_along(x)) {
    x[[i]] <- if (is.null(x[[i]]) || x[[i]] %in% replace) y[[i]] else x[[i]]
  }
  
  return(x)
}


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


