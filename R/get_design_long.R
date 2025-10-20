#' Get design from long data
#' 
#' Makes a best guess at the design of a long-format data frame. 
#' 
#' Finds all columns that contain a single value per unit of analysis (between factors), 
#' all columns that contain the same values per unit of analysis (within factors), and 
#' all columns that differ over units of analysis (dv, continuous factors)
#' 
#' @param data the data frame (in long format)
#' @param dv the column name that identifies the DV
#' @param id the column name(s) that identify a unit of analysis
#' @param plot whether to show a plot of the design
#' 
#' @return a design list
#' 
#' @export
#'
get_design_long <- function(data, 
                            dv = c(y = "score"), 
                            id = c(id = "id"), 
                            plot = faux_options("plot")) {
  if (is.null(names(id))) names(id) <- id
  if (is.null(names(dv))) names(dv) <- dv
  
  # check for columns where there is only ever one value per id
  y <- by(data, data[names(id)], function(x) {
    unique_vals <- lapply(x, unique)
    n_unique_vals <- lapply(unique_vals, length)
    as.data.frame(n_unique_vals)
  })
  
  z <- do.call(rbind, y)
  factors <- lapply(z, function(x) {
    ifelse(max(x) > 1, "W", "B")
  })
  
  # get rid of id and dv columns
  cnames <- setdiff(names(data), c(names(id), names(dv)))
  factors <- factors[names(factors) %in% cnames]
  
  between_factors <- names(factors[which(factors == "B")])
  within_factors <- names(factors[which(factors == "W")])
  
  # get levels for each column
  lvls <- data[, names(data) %in% cnames, drop = FALSE]
  lvls <- lapply(lvls, unique)
  lvls <- lapply(lvls, as.character)
  lvls <- lapply(lvls, fix_name_labels)
  
  within <- lvls[which(names(lvls) %in% within_factors)]
  between <- lvls[which(names(lvls) %in% between_factors)]
  
  # define columns
  cells_w <- cell_combos(within, names(dv))
  cells_b <- cell_combos(between, names(dv)) 
  
  # get n, mu, sd, r per cell
  chk <- sample_params(data, between_factors, within_factors, 
                    names(dv), names(id), digits = 8)
  
  if (length(between_factors)) {
    chk_b <- chk

    x <- chk[between_factors]; x$sep = "_";
    b <- do.call(paste, x)
    chk_b$`.between` <- factor(b, cells_b)
    chk_b <- chk_b[order(chk_b$`.between`), , drop = FALSE]
    chk_b[between_factors] <- NULL
  } else {
    chk_b <- chk
    chk_b$`.between` <- names(dv)
  }
  
  get_stat <- function(stat) {
    x <- as.data.frame(chk_b[, c(".between", within_factors, stat)])
    # y <- stats::reshape(x, timevar = within_factors, 
    #                     idvar = ".between", 
    #                     direction = "wide")
    # rownames(y) <- y$`.between`
    # y$`.between` <- NULL
    # names(y) <- gsub(paste0(stat, "\\."), "", names(y))
    # y <- y[, order(cells_w)] # FIX: check if needed?
    # y
    x[[stat]]
  }
  
  n <- get_stat("n")
  sd <- get_stat("sd")
  mu <- get_stat("mean")
  
  x <- chk_b[, c(".between", within_factors, cells_w)] %>% as.data.frame()
  r <- by(x, x$`.between`, function(y) {
    rownames(y) <- cells_w
    y <- y[cells_w, cells_w]
    as.matrix(y)
  })
  
  attr(r, "call") <- NULL
  class(r) <- "list"
  
  check_design(within = within, between = between, 
               n = n, mu = mu, sd = sd, r = r, 
               dv = dv, id = id, plot = plot)
}

