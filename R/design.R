#' Validates the specified design
#' 
#' Specify any number of within- and between-subject factors with any number of levels. 
#' 
#' Specify n for each between-subject cell; mu and sd for each cell, and r for the within-subject cells for each between-subject cell.
#' 
#' This function returns a validated design list for use in sim_data to simulate a data table with this design, or to archive your design.
#' 
#' See [`vignette("sim_design", package = "faux")`](../doc/sim_design.html) for details.
#' 
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu a vector giving the means of the variables
#' @param sd the standard deviations of the variables
#' @param r the correlations among the variables (can be a single number, full correlation matrix as a matric or vector, or a vector of the upper right triangle of the correlation matrix
#' @param dv the name of the DV column list(y = "value")
#' @param id the name of the ID column list(id = "id")
#' @param vardesc a list of variable descriptions having the names of the within- and between-subject factors
#' @param plot whether to show a plot of the design
#' @param design a design list including within, between, n, mu, sd, r, dv, id
#' @param fix_names deprecated
#' @param sep separator for factor levels
#' 
#' @return list
#' 
#' @examples 
#' 
#' within <- list(time = c("day", "night"))
#' between <- list(pet = c("dog", "cat"))
#' mu <- list(dog = 10, cat = 5)
#' vardesc <- list(time = "Time of Day", pet = "Type of Pet")
#' check_design(within, between, mu = mu, vardesc = vardesc)
#' 
#' between <- list(language = c("dutch", "thai"),
#'                 pet = c("dog", "cat"))
#' mu <- list(dutch_dog = 12, dutch_cat = 7, thai_dog = 8, thai_cat = 3)
#' check_design(within, between, mu = mu)
#' @export
#' 
check_design <- function(within = list(), between = list(), 
                         n = 100, mu = 0, sd = 1, r = 0, 
                         dv = list(y = "value"), 
                         id = list(id = "id"), 
                         vardesc = list(),
                         plot = faux_options("plot"), 
                         design = NULL, fix_names = FALSE,
                         sep = faux_options("sep")) {
  # design passed as design list
  if (!is.null(design) && is.list(design)) {
    # double-check the entered design
    list2env(design, envir = environment())
  } else if ("design" %in% class(within)) {
    # design given as first argument: not ideal but handle it
    list2env(within, envir = environment())
  }
  
  # name anonymous factors ----
  if (is.numeric(within) && all(within %in% 2:20)) { # vector of level numbers
    within_names <- paste0("W", 1:length(within))
    indices <- lapply(within, function(.) letters[seq(1:.)])
    within <- mapply(paste0, within_names, indices, SIMPLIFY = FALSE)
  }
  if (is.numeric(between) && all(between %in% 2:20)) { # vector of level numbers
    between_names <- paste0("B", 1:length(between))
    indices <- lapply(between, function(.) letters[seq(1:.)])
    between <- mapply(paste0, between_names, indices, SIMPLIFY = FALSE)
  }
  
  # check factor specification ----
  if (!is.list(within) || !is.list(between)) {
    stop("within and between must be lists")
  }
  
  # if within or between factors are named vectors, 
  # use their names as column names and values as labels for plots
  between <- lapply(between, fix_name_labels, pattern = NULL)
  within <- lapply(within, fix_name_labels, pattern = NULL)
  dv <- fix_name_labels(dv, pattern = NULL)
  id <- fix_name_labels(id, pattern = NULL)
  
  # check if sep is in any level names
  all_levels <- lapply(c(between, within), names) %>% 
    unlist() %>% unname()
  has_sep <- grepl(sep, all_levels, fixed = TRUE)
  if (any(has_sep)) {
    # see which separators are safe
    seps <- c("_", ".", "-", "~", "_x_", "*")
    names(seps) <- seps
    safe_sep <- lapply(seps, grepl, x = all_levels, fixed = TRUE) %>%
      sapply(any)
    
    stop("These level names have the separator '", sep, "' in them: ", 
         paste(all_levels[has_sep], collapse = ", "),
         "\nPlease change the names (see fix_name_labels) or choose another separator character using the sep argument or faux_options(sep = '~')\n",
         "safe separators for your factor labels are: ",
         paste(seps[safe_sep], collapse = ","))
  }
  
  # if (fix_names) {
  #   pattern <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", sep)
  #   between <- lapply(between, fix_name_labels, pattern = pattern)
  #   within <- lapply(within, fix_name_labels, pattern = pattern)
  # }
  
  # check for duplicate factor names ----
  all_names <- c(names(within), names(between))
  factor_overlap <- duplicated(all_names)
  if (sum(factor_overlap) > 0) {
    dupes <- unique(all_names[factor_overlap])
    stop("You have multiple factors with the same name (", 
         paste(dupes, collapse = ", "),
         "). Please give all factors unique names.")
  }
  
  # check vardesc ----
  if (length(vardesc) == 0) {
    vardesc <- stats::setNames(all_names, all_names)
  } else {
    # handle missing or extra names
    missing_names <- setdiff(all_names, names(vardesc))
    if (length(missing_names) > 0) {
      warning("vardesc is missing definitions for factors: ",
              paste(missing_names, collapse = ", "))
    }
    missing_vardesc <- stats::setNames(missing_names, missing_names)
    inclued_names <- intersect(all_names, names(vardesc))
    included_vardesc <- vardesc[inclued_names]
    vardesc <- c(included_vardesc, missing_vardesc)
  }
  vardesc <- as.list(vardesc)
  
  # check for duplicate level names within any factor ----
  dupes <- c(within, between) %>%
    lapply(duplicated) %>%
    lapply(sum) %>%
    lapply(as.logical) %>%
    unlist()
  
  if (sum(dupes)) {
    dupelevels <- c(within, between) %>% 
      names() %>% 
      `[`(dupes) %>% 
      paste(collapse = ", ")
    stop("You have duplicate levels for factor(s): ", dupelevels)
  }
  
  # define columns ----
  cells_w <- cell_combos(within, names(dv), sep)
  cells_b <- cell_combos(between, names(dv), sep) 
  
  # convert n, mu and sd  ----
  
  if (is.atomic(n) && !is.matrix(n) && is.null(names(n)) && length(n) > 1) {
    n <- as.list(n) # unnamed vector Ns have problems if there are within factors
  }
  cell_n  <- convert_param(n, cells_w, cells_b, "Ns")
  for (i in names(cell_n)) {
    cell_n[[i]] <- cell_n[[i]][[1]]
  }
  cell_mu <- convert_param(mu, cells_w, cells_b, "means")
  cell_sd <- convert_param(sd, cells_w, cells_b, "SDs")
  
  # set up cell r from r ----
  # (number, vector, matrix or list styles)
  cell_r <- list()
  if (length(within)) {
    for (cell in cells_b) {
      cell_cor <- if(is.list(r)) r[[cell]] else r
      mat <- cormat(cell_cor, length(cells_w))
      rownames(mat) <- cells_w
      colnames(mat) <- cells_w
      cell_r[[cell]] <- mat
    }
  }
  
  # check n ----
  n <- suppressWarnings(lapply(n, as.numeric)) # make sure all cells are numbers
  if (unlist(n) %>% is.na() %>% sum()) { stop("All n must be numbers") }
  if (sum(unlist(n) %% 1 > 0)) {
    warning("Some cell Ns are not integers. They have been rounded up to the nearest integer.")
    n <- lapply(n, ceiling)
  }
  if (sum(unlist(n) < 0)) { stop("All n must be >= 0") }
  if (sum(unlist(n) == 0)) {
    warning("Some cell Ns are 0. Make sure this is intentional.")
  }
  
  # check mu ----
  mu <- suppressWarnings(lapply(mu, as.numeric)) # make sure all cells are numbers
  if (unlist(mu) %>% is.na() %>% sum()) { stop("All mu must be numbers") }
  
  # check sd ----
  sd <- suppressWarnings(lapply(sd, as.numeric)) # make sure all cells are numbers
  if (unlist(sd) %>% is.na() %>% sum()) { stop("All sd must be numbers") }
  if (sum(unlist(sd) < 0)) { stop("All sd must be >= 0") }
  
  # start param table ----
  fnames <- lapply(c(between, within), names)
  d <- do.call(expand.grid, rev(fnames))
  
  if (nrow(d) == 0 & ncol(d) == 0) {
    d <- as.data.frame(dv)
  } else {
    d <- d[,ncol(d):1] %>% as.data.frame()
  }
  
  if (length(within)) {
    rmat <- cell_r %>% unlist() %>% unname() %>% 
      matrix(ncol = length(cells_w), byrow = TRUE)
    colnames(rmat) <- cells_w
    
    for (w in cells_w) { d[w] <- rmat[,w] }
  }
  
  d$n <- unlist(cell_n) %>% rep(each = length(cells_w))
  d$mu <- unlist(cell_mu)
  d$sd <- unlist(cell_sd)
  
  design <- list(
    within = within,
    between = between,
    dv = dv,
    id = id,
    vardesc = vardesc,
    n = cell_n,
    mu = cell_mu,
    sd = cell_sd,
    r = cell_r,
    sep = sep,
    params = d
  )
  
  class(design) <- c("design", "list")
  
  if (plot) { plot_design(design) %>% print() }
  
  invisible(design)
}

#' Print Design List
#'
#' @param x The design list
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#' @returns Prints x and returns it invisibly
#'
print.design <- function(x, ...) {
  if (!"quote" %in% names(list(...))) quote <- ""
  
  # add factor labels from vardesc
  if (!is.null(x$vardesc)) {
    wn <- names(x$within)
    wv <- x$vardesc[wn]
    if (!setequal(wn, wv)) names(x$within) <- paste0(wn, ": ", wv)
    bn <- names(x$between)
    bv <- x$vardesc[bn]
    if (!setequal(bn, bv)) names(x$between) <- paste0(bn, ": ", bv)
  }
  
  txt <- "" #"Design\n\n"
  txt <- sprintf("%s* [DV] %s: %s%s%s  \n", 
                 txt, names(x$dv), quote, x$dv, quote)
  txt <- sprintf("%s* [ID] %s: %s%s%s  \n", 
                 txt, names(x$id), quote, x$id, quote)
  txt <- sprintf("%s* Within-subject variables:\n%s\n", 
                 txt, nested_list(x$within, quote = quote, pre = "    "))
  txt <- sprintf("%s* Between-subject variables:\n%s\n", 
                 txt, nested_list(x$between, quote = quote, pre = "    "))
  txt <- knitr::kable(x$params) %>%
    paste(collapse = "\n    ") %>%
    sprintf("%s* Parameters:\n    %s\n\n",  txt, .)
  
  cat(txt)
}


#' Get design
#' 
#' Get the design specification from a data table created in faux. This can be used to create more simulated data with the same design.
#'
#' @param data The data table to check
#'
#' @return list with class design
#' @export
#'
#' @examples
#' data <- sim_design(2, 2, plot = FALSE)
#' design <- get_design(data)
#' data2 <- sim_design(design, plot = FALSE)
get_design <- function(data) {
  design <- attributes(data)$design %||% list()
}


#' Set design
#' 
#' Add a design specification to a data table
#'
#' @param data The data table
#' @param design The design list
#'
#' @return A data frame with a design attribute
#' @export
#'
#' @examples
#' design <- check_design()
#' data <- data.frame(id = 1:100, y = rnorm(100)) %>%
#'   set_design(design)
set_design <- function(data, design) {
  attr(data, "design") <- design
  class(data) <- c("faux", "data.frame")
  
  data
}


#' Convert design to JSON
#' 
#' Convert a design list to JSON notation for archiving (e.g. in scienceverse)
#'
#' @param design a design list including within, between, n, mu, sd, r, dv, id
#' @param filename option name of file to save the json to
#' @param digits number of digits to save
#' @param pretty whether to print condensed or readable
#' @param ... other options to send to jsonlite::toJSON
#'
#' @return a JSON string
#' @export
#'
#' @examples
#' des <- check_design(2,2)
#' json_design(des)
#' json_design(des, pretty = TRUE)
json_design <- function(design, filename = NULL, 
                        digits = 8, pretty = FALSE, ...) {
  valid_design <-  check_design(design = design, plot = FALSE)
  valid_design$params <- NULL
  
  j <- jsonlite::toJSON(valid_design, auto_unbox = TRUE, digits = digits, pretty = pretty, ...)
  
  if (!is.null(filename)) {
    # fix filename
    if (!length(grep("\\.json$", filename))) {
      # add .json extension if not already specified
      filename <- paste0(filename, ".json")
    }
  
    writeLines(j, filename)
  }
  
  j
}

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



# new design functions ----

new_design <- function(id = c(id = "ID"), 
                       dv = c(y = "DV"), 
                       within = list(), 
                       between = list(), 
                       labels = list(),
                       dist = "norm",
                       params = NULL,
                       r = 0) {
  d <- list(
    id = id,
    dv = dv,
    within = within,
    between = between,
    labels = labels,
    dist = dist,
    params = params,
    r = r
  )
  class(d) <- c("design")
  
  return(d)
}

validate_design <- function(design = new_design(), ...) {
  if (is.list(design)) list2env(design, envir = environment())
  
  # overwrite design with any named values
  list2env(list(...), envir = environment())
  
  # validate each aspect ----
  
  ## set names of vectors if missing ----
  names(id) <- rep_if(names(id) %||% id, id, "")
  names(dv) <- rep_if(names(dv) %||% dv, dv, "")
  within <- lapply(within, \(x) rep_if(names(x) %||% x, x, ""))
  between <- lapply(between, \(x) rep_if(names(x) %||% x, x, ""))
  
  new_design(id = id,
             dv = dv,
             within = within,
             between = between,
             labels = labels,
             dist = dist,
             params = params,
             r = r)
}

design <- function(id = c(id = "ID"), 
                   dv = c(y = "DV"), 
                   within = list(), 
                   between = list(), 
                   labels = list(),
                   dist = "norm",
                   params = NULL,
                   r = 0) {
  d <- validate_design(id = id,
                  dv = dv,
                  within = within,
                  between = between,
                  labels = labels,
                  dist = dist,
                  params = params,
                  r = r)
  return(d)
}