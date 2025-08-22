#' Simulate data from design
#'
#' Generates a data table with a specified within and between design. See \href{https://scienceverse.github.io/faux/articles/sim_design.html}{\code{vignette("sim_design", package = "faux")}} for examples and details.
#'
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu the means of the variables
#' @param sd the standard deviations of the variables
#' @param r the correlations among the variables (can be a single number, full correlation matrix as a matrix or vector, or a vector of the upper right triangle of the correlation matrix
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param long Whether the returned tbl is in wide or long format (defaults to value of `faux_options("long")`)
#' @param dv the name of the dv for long plots (defaults to y)
#' @param id the name of the id column (defaults to id)
#' @param vardesc a list of variable descriptions having the names of the within- and between-subject factors
#' @param plot whether to show a plot of the design
#' @param interactive whether to run the function interactively
#' @param design a design list including within, between, n, mu, sd, r, dv, id, and vardesc
#' @param rep the number of data frames to return (default 1); if greater than 1, the returned data frame is nested by rep (if nested = TRUE)
#' @param nested Whether to nest data frames by rep if rep > 1
#' @param seed DEPRECATED use set.seed() instead before running this function
#' @param sep separator for factor levels
#' 
#' @return a tbl
#' 
#' @export
#' 
sim_design <- function(within = list(), between = list(), 
                       n = 100, mu = 0, sd = 1, r = 0, 
                       empirical = FALSE, 
                       long = faux_options("long"), 
                       dv = list(y = "value"), 
                       id = list(id = "id"),
                       vardesc = list(),
                       plot = faux_options("plot"), 
                       interactive = FALSE, 
                       design = NULL, rep = 1, nested = TRUE, 
                       seed = NULL, sep = faux_options("sep")) {
  # check the design is specified correctly
  if (interactive) {
    design <- interactive_design(plot = plot)
  } else if (!is.null(design)) { 
    #design <- check_design(design = design, plot = plot, sep = design$sep)
  } else if ("design" %in% class(within)) {
    # design given as first argument: not ideal but handle it
    #design <- check_design(design = within, plot = plot, sep = within$sep)
    design <- within
  } else {
    design <- check_design(within = within, between = between, 
                         n = n, mu = mu, sd = sd, r = r, 
                         dv = dv, id = id, vardesc = vardesc,
                         plot = plot, sep = sep)
  }
  
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() before running sim_design() instead")
  }
  
  # simulate the data
  data <- sim_data(design, empirical = empirical, 
                   long = long, 
                   rep = rep, nested = nested) %>%
    set_design(design)
  
  return(data)
}

#' Simulate data from design (internal)
#'
#' @param design A list of design parameters created by check_design()
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param long Whether the returned tbl is in wide (default = FALSE) or long (TRUE) format
#' @param rep the number of data frames to return (default 1); if greater than 1, the returned data frame is nested by rep
#' @param nested Whether to nest data frames by rep if rep > 1
#' @param seed DEPRECATED use set.seed() instead before running this function
#' 
#' @return a tbl
#' @keywords internal
#' 
sim_data <- function(design, empirical = FALSE, 
                     long = faux_options("long"), 
                     rep = 1, nested = TRUE, seed = NULL) {
  if (!is.numeric(rep)) {
    stop("rep must be a number")
  } else if (rep < 1) {
    stop("rep must be >= 1")
  } else if (rep%%1 != 0) {
    warning("rep should be an integer")
  }
  
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() instead")
  #   # reinstate system seed after simulation
  #   gs <- global_seed(); on.exit(global_seed(gs))
  #   set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  }
  r <- n <- mu <- sep <- between <- within <- NULL # stops global def error in check 
  list2env(design, envir = environment())
  
  # set sep with within cells to something weird if it will never show up
  wsep <- ifelse(long, "|~|", sep)
  
  # only use DV and ID names here
  dv <- names(dv)
  id <- names(id)
  
  # define columns
  cells_w <- cell_combos(within, dv, wsep)
  cells_b <- cell_combos(between, dv, sep) 
  cells_b2 <- cell_combos(between, dv, wsep)
  names(cells_b2) <- cells_b
  
  # get factor names
  within_factors <- names(within)
  between_factors <- names(between)
  
  # handle no w/in or btwn
  if (length(between_factors) == 0) between_factors <- ".tmpvar."
  if (length(within_factors) == 0)  within_factors  <- ".tmpvar."
  
  # simulate data for each between-cell
  for (rep_n in 1:rep) {
    for (cell in cells_b) {
      cell_vars <- rnorm_multi(
        n = n[[cell]], 
        vars = length(cells_w), 
        mu = mu[[cell]] %>% unlist(), 
        sd = sd[[cell]] %>% unlist(), 
        r = r[[cell]], 
        varnames = cells_w, 
        empirical = empirical
      )
      cell_vars$.btwn. <- cells_b2[cell]
      
      # add cell values to df
      if (cell == cells_b[1]) { 
        sub_df <- cell_vars # first cell sets up the df
      } else {
        sub_df <- rbind(sub_df, cell_vars)
      }
    }
    sub_df$`.rep.` <- rep_n
    if (rep_n == 1) {
      df <- sub_df
    } else {
      df <- rbind(df, sub_df)
    }
  }
  
  # set column order
  col_order <- c(".rep.", id, between_factors, cells_w) %>%
    setdiff(c(".tmpvar."))
  
  # figure out total number of subjects
  sub_n <- unlist(n) %>% sum()
  
  # create wide dataframe
  btwn <- strsplit(df$.btwn., wsep, fixed = TRUE) %>% 
    unlist() %>% matrix(nrow = length(between_factors)) %>% 
    t() %>% as.data.frame()
  names(btwn) <- between_factors
  # col_types <- lapply(between, `[[`, 1) %>% 
  #   sapply(typeof) %>%
  #   sapply(substr, 1, 1) %>%
  #   paste(collapse = "")
  btwn <- utils::type.convert(btwn, as.is = FALSE)
  
  df_wide <- cbind(df, btwn)
  df_wide$.btwn. <- NULL
  df_wide <- by(df_wide, df_wide$.rep., function(x) {
    x[id] <- make_id(sub_n); x
  }) %>% do.call(rbind, .)
  df_wide <- df_wide[col_order]
  
  # put factors in order
  factors_to_order <- setdiff(between_factors, ".tmpvar.")
  for (f in factors_to_order) {
    if (typeof(between[[f]][[1]]) == "character") {
      df_wide[[f]] <- factor(df_wide[[f]], levels = names(between[[f]]))
    }
  }
  
  if (long == TRUE && length(within)) {
    # not necessary for fully between designs
    col_order <- c(".rep.", id, between_factors, within_factors, dv) %>%
      setdiff(".tmpvar.")
    
    df_long <- stats::reshape(
      df_wide, cells_w, direction = "long", 
      idvar = c(".rep.", id),  v.names = dv,
      timevar = ".win.")
    
    w_in <- cells_w[df_long$.win.] %>%
      strsplit(wsep, fixed = TRUE) %>% 
      unlist() %>% matrix(nrow = length(within_factors)) %>% 
      t() %>% as.data.frame()
    names(w_in) <- within_factors
    w_in <- utils::type.convert(w_in, as.is = FALSE)
    df_long$.win. <- NULL
    df_long <- cbind(df_long, w_in)[col_order]
    
    # put factors in order
    factors_to_order <- setdiff(within_factors, ".tmpvar.")
    for (f in factors_to_order) {
      if (typeof(within[[f]][[1]]) == "character") {
        df_long[[f]] <- factor(df_long[[f]], levels = names(within[[f]]))
      }
    }

    df_return <- df_long
  } else {
    df_return <- df_wide
  }
  
  class(df_return) <- c("faux", "data.frame")
  
  if (rep == 1) {
    df_return$.rep. <- NULL
  } else if (isTRUE(nested)) {
    # nest reps
    df_rep <- by(df_return, df_return$.rep., function(x) {
      x$.rep. <- NULL
      x
    })
    df_return <- data.frame(rep = 1:rep)
    df_return$data <- df_rep # can't assign list in data.frame
  } else {
    rep_index <- which(names(df_return) == ".rep.")
    names(df_return)[rep_index] <- "rep"
    df_return <- df_return[order(df_return$rep),] 
  }
  
  rownames(df_return) <- c() # get rid of row names
  return(df_return)
}
