#' Convert data from long to wide format
#' 
#' @param data the tbl in long format
#' @param within the names of the within column(s)
#' @param between the names of between column(s) (optional)
#' @param dv the name of the DV (value) column
#' @param id the names of the column(s) for grouping observations
#' @param sep separator for factor levels
#' 
#' @return a tbl in wide format
#' 
#' @examples 
#' df_long <- sim_design(2, 2, long = TRUE)
#' long2wide(df_long, "A", "B")
#' 
#' @export
#' 
long2wide <- function(data, within = c(), between = c(), dv = "y", id = "id", sep = faux_options("sep")) {
  if ("design" %in% names(attributes(data))) {
    # get parameters from design
    design <- get_design(data)
    
    within <- names(design$within)
    between <- names(design$between)
    dv <- names(design$dv)
    id <- names(design$id)
    sep <- design$sep
  } else {
    #design <- get_design_long(data, dv = dv, id = id, plot = FALSE)
  }
  
  if (length(within) == 0) return(data)
  
  d1 <- data[c(id, between, within, dv)] %>% as.data.frame()
  sep_replace <- "~-~" # avoid odd parsing
  sep_escape <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", sep)
  for (w in within) d1[w] <- gsub(sep_escape, sep_replace, d1[[w]])
  
  tmpw <- d1[within]
  tmpw$sep <- sep
  d1$.tmpwithin. <- do.call(paste, tmpw)
  d1[within] <- NULL

  d1 <- stats::reshape(d1, idvar = c(id, between), 
                      timevar = ".tmpwithin.", 
                      direction = "wide")
  pat <- paste0("^", dv, "\\.")
  names(d1) <- gsub(pat, "", names(d1))
  
  # FIX: get levels from design if available
  for (b in between) d1[[b]] <- factor(d1[[b]])
  
  # fix names 
  names(d1) <- gsub(sep_replace, sep, names(d1))
  
  if ("design" %in% names(attributes(data))) {
    attributes(d1)$design <- design
  }
  class(d1) <- c("faux", "data.frame")
  rownames(d1) <- NULL
  d1 <- add_labels(d1)
  
  d1
}

