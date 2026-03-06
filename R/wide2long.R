#' Convert data from wide to long format
#' 
#' @param data the tbl in wide format
#' @param within_factors the names of the within factors
#' @param within_cols the names (or indices) of the within-subject (value) columns
#' @param dv the name of the dv column (defaults to "y")
#' @param id the name of the ID column(s) if they don't exist, a new column will be made (defaults to ("id")
#' @param sep separator for within-columns (to be used in strsplit, so can be regex), defaults to "_"
#' 
#' @return a tbl in long format
#' 
#' @examples 
#' wide2long(iris, c("Feature", "Measure"), 1:4, sep = "\\.")
#' 
#' @export
#' 
wide2long <- function(data, within_factors = c(), within_cols = c(), 
                      dv = "y", id = "id", sep = faux_options("sep")) {
  if ("design" %in% names(attributes(data))) {
    # get parameters from design
    design <- get_design(data)
    
    dv <- names(design$dv)
    id <- names(design$id)
    within_factors <- names(design$within)
    within_cols <- cell_combos(design$within, dv) 
  } else {
    design <- NULL
  }
  
  # a full stop never makes sense, so escape
  if (sep == ".") sep <- "\\."
  
  if (is.numeric(within_cols)) within_cols <- names(data)[within_cols]
    
  # check if ID exists and make if not
  if (!(id %in% names(data))) {
    data[[id]] <- make_id(nrow(data))
  }

  df_long <- stats::reshape(data, within_cols, direction = "long", 
    idvar = id,  v.names = dv, 
    timevar = ".win.")
  
  w_in <- within_cols[df_long$.win.] %>%
    strsplit(sep) %>% 
    unlist() %>% matrix(nrow = length(within_factors)) %>% 
    t() %>% as.data.frame()
  names(w_in) <- within_factors
  df_long$.win. <- NULL
  btwn <- setdiff(names(df_long), c(id, dv))
  col_ord <- c(id, btwn, within_factors, dv)
  longdat <- cbind(df_long, w_in)[col_ord]
  
  # make new factors into factors
  for (wf in within_factors) {
    if (is.null(design)) {
      levels <- unique(longdat[[wf]])
    } else {
      levels <- names(design$within[[wf]])
    }
    longdat[[wf]] <- factor(longdat[[wf]], levels, levels)
  }
  
  attributes(longdat)$design <- design
  class(longdat) <- c("faux", "data.frame")
  rownames(longdat) <- NULL
  longdat <- add_labels(longdat)
  
  longdat
}

