#' Check readline input
#'
#' @param prompt the prompt for readline
#' @param type what type of check to perform, one of  c("numeric", "integer", "length", "grep")
#' @param min the minimum value
#' @param max the maximum value
#' @param warning an optional custom warning message
#' @param default the default option to return if the entry is blank, NULL allows no default, the default value will be displayed after the text as default
#' @param ... other arguments to pass to grep
#'
#' @return the validated result of readline
#' @export
#'
#' @examples
#' if(interactive()){
#' readline_check("Type a number: ", "numeric")
#' readline_check("Type two characters: ", "length", min = 2, max = 2)
#' readline_check("Type at least 3 characters: ", "length", min = 3)
#' readline_check("Type no more than 4 characters: ", "length", max = 44)
#' readline_check("Type a letter and a number: ", "grep", pattern = "^[a-zA-Z]\\d$")
#' }
readline_check <- function(prompt, type = c("numeric", "integer", "length", "grep"), 
                           min = -Inf, max = Inf, warning = NULL, default = NULL, ...) {
  con <- getOption("faux.connection", stdin())
  if (!is.null(default)) prompt <- sprintf("%s [%s]", prompt, default)
  cat(paste0(prompt, "\n"))
  input <- readLines(con = con, n = 1)
  
  if (!is.null(default) & (length(input) == 0 || input == "")) {
    return(default)
  }
  
  type <- match.arg(type)
  if (type == "numeric") {
    if (min != -Inf | max != Inf) {
      warn_text <- paste0("The input must be a number between ", min, " and ", max, ":")
    } else {
      warn_text <- "The input must be a number:"
    }
    input <- suppressWarnings(as.numeric(input))
    check <- !is.na(input)
    check <- check & (input >= min) & (input <= max)
  } else if (type == "integer") {
    if (min != -Inf | max != Inf) {
      warn_text <- paste0("The input must be an integer between ", min, " and ", max, ":")
    } else {
      warn_text <- "The input must be an integer:"
    }
    check <- grep("^\\d+$", input) %>% length() > 0
    input <- suppressWarnings(as.integer(input))
    check <- check & (input >= min) & (input <= max)
  } else if (type == "length") {
    min <- max(min, 0) # min can't be smaller than 0 for text
    warn_text <- paste0("The input must be between ", min, " and " , max, " characters long:")
    check <- (nchar(input) >= min) & (nchar(input) <= max)
  } else if (type == "grep") {
    warn_text <- "The input is incorrect:"
    check <- grep(x = input, ...) %>% length() > 0
  } else {
    warn_text <- "The input is incorrect:"
    check <- FALSE # default false if type is wrong?
  }
  
  # custom warning text
  if (!is.null(warning)) { 
    warn_text = warning
  }
  
  # add red Error start
  warn_text <- paste0("\033[31mError:\033[39m ", warn_text)
  
  if (!check) {
    Recall(warn_text, type, min, max, warning, ...)
  } else {
    input
  }
}

