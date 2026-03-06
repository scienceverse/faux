#' Add random effects to a data frame
#'
#' @param .data the data frame
#' @param .by the grouping column (groups by row if NULL)
#' @param ... the name and standard deviation of each random effect
#' @param .cors the correlations among multiple random effects, to be passed to [rnorm_multi()] as r
#' @param .empirical logical. To be passed to [rnorm_multi()] as empirical
#'
#' @return data frame with new random effects columns
#' @export
#'
#' @examples
#' add_random(rater = 2, stimulus = 2, time = 2) %>%
#'   add_ranef("rater", u0r = 1.5) %>%
#'   add_ranef("stimulus", u0s = 2.2, u1s = 0.75, .cors = 0.5) %>%
#'   add_ranef(c("rater", "stimulus"), u0sr = 1.2)
add_ranef <- function(.data, .by = NULL, ..., .cors = 0, .empirical = FALSE) {
  if (is.null(.by)) {
    .by <- names(.data)
    grps <- .data
  } else {
    grps <- unique(.data[.by])
  }
  sd <- c(...)
  
  ranefs <- faux::rnorm_multi(
    n = nrow(grps),
    sd = sd,
    vars = length(sd),
    r = .cors,
    empirical = .empirical
  ) %>%
    dplyr::bind_cols(grps)
  
  dplyr::left_join(.data, ranefs, by = .by)
}

#' Add a dependent variable to a fixed-effect model
#'
#' @param .data the data frame
#' @param vars a vector of the DV(s) to create
#' @param between specifies the between cells
#' @param mean the mean(s) for the DV(s) - ignored if params is set
#' @param sd the SD(s) for the DV(s) - ignored if params is set
#' @param r the correlation for the DVs (if >1 DV)
#' @param dist the distribution(s) for the DVs
#' @param params the distribution parameters
#' @param empirical logical. If true, mu, sd, r and params specify the empirical not population statistics
#'
#' @returns a data frame with new DV column(s)
#' @export
#'
#' @examples
#' df <- add_random(id = 1000) |>
#'   add_between(condition = c("ctl", "exp")) |>
#'   add_dv(vars = c(pre = "Pre-test Score", post = "Post-test Score"),
#'          between = list(condition = "ctl"),
#'          mean = c(100, 105),
#'          sd = 10,
#'          r = 0.5,
#'          ) |>
#'   add_dv(vars = c(pre = "Pre-test Score", post = "Post-test Score"),
#'          between = list(condition = "exp"),
#'          mean = c(100, 110),
#'          sd = c(10, 8),
#'          r = 0.4)
add_dv <- function(.data, 
                   vars = c("dv" = "Dependent Variable"), 
                   between = list(),
                   mean = 0,
                   sd = 1,
                   r = 0, 
                   dist = "norm",
                   params = list(), 
                   empirical = FALSE) {
  names(vars) <- names(vars) %||% unlist(vars)
  
  design <- get_design(.data) %||% list()
  design$dv <- vars
  attr(.data, "design") <- design
  if (length(design$within) > 0 & 
      all(names(design$within) %in% names(.data))) {
    # make data wide
    if (!all(names(vars) %in% names(.data))) {
      # add new dv
      .data[, names(vars)] <- NA_real_
    }
    
    .data <- long2wide(.data)
    vars <- cell_combos(design$within, sep = design$sep)
  } else {
    vars <- names(vars)
  }
  
  # check all columns exist
  cols <- names(between)
  wrong_col <- setdiff(cols, names(.data))
  if (length(wrong_col)) {
    stop(paste(wrong_col, collapse = ", "), " is not a column")
  }
  
  # filter .data to 
  subdat <- rep(TRUE, nrow(.data))
  for (x in cols) {
    subdat <- subdat & (.data[[x]] %in% between[[x]])
  }
  
  n <- sum(subdat)
  vlen <- length(vars)
  
  # make params if mean/sd used and all normal
  if (all(dist == "norm") & length(params) == 0) {
    mean <- rep_len(mean, vlen)
    sd <- rep_len(sd, vlen)
    for (i in seq_along(vars)) {
      varname <- vars[[i]]
      params[[varname]] <- list(mean = mean[[i]], sd = sd[[i]])
    }
  }
  
  if (length(dist) != vlen) {
    dist <- rep_len(dist, vlen)
  }
  names(dist) <- names(dist) %||% vars
  
  .data[subdat, names(dist)] <- rmulti(n, dist, params, r, empirical, as.matrix = TRUE)
  
  if (length(design$within)) {
    .data <- wide2long(.data)
  }
  
  .data
}


#' Add a dependent variable to a mixed effects model
#' 
#' Add a dependent variable to a mixed model simulation using a formula and specification of fixed and random effects parameters.
#' 
#' Fixed effects are specified as a named list for each effect in the equation. For example, for the equation `y ~ a * b + (1 | id)`, the fixed effects might be specified as such: `list(a = 5, b = 10, "a:b" = 0)`.
#' 
#' Random effects are also specified as a named list of standard deviations for the random intercept and slopes, plus optional correlations. For example, for the equation `y ~ a * b + (b | id)`, the random effects might be specified as such: `list(id = list(intercept = 10, b = 5, .cors = 0.4))`. 
#'
#' @param .data the data frame
#' @param formula The formula for your model
#' @param intercept The (grand) intercept value
#' @param error The SD of the error term
#' @param fixef A list of fixed effects (see Details)
#' @param ranef A list of random effects parameters (see Details)
#'
#' @return a data frame with new DV column
#' @keywords internal
#'
#' @examples
#' add_random(id = 1000) |> 
#'   add_between(a = c("A1", "A2")) |>
#'   add_within(b = c("B1", "B2")) |>
#'   add_dv_mixed(y ~ a*b + (b | id),
#'          intercept = 100,
#'          error = 10,
#'          fixef = list(a = 5, b = 10, "a:b" = 0),
#'          ranef = list(id = list(intercept = 10, b = 5, .cors = 0.4))
#'   )
# add_dv_mixed <- function(.data, formula = y ~ 1, 
#                    intercept = 0, 
#                    error = 1,
#                    fixef = list(), 
#                    ranef = list()) {
# if (is.character(formula)) formula <- stats::as.formula(formula)
# dv <- all.vars(formula[[2]])
# .data[dv] <- 0
# m <- lm(formula, .data)
# 
# conames <- names(m$coefficients)
# coefs <- c(intercept)
# m$coefficients <- setNames(coefs, conames)
# err <- rnorm(nrow(.data), 0, error)
# .data[dv] <- predict(m) + err
#   
#   return(.data)
# }

#' Add column labels
#'
#' @param .data the data frame
#' @param ... the column names and lables (e.g., `id = "Student ID"`)
#'
#' @return data frame with column labels
#' @export
#'
#' @examples
#' df <- add_random(rid = 2, sid = 2) |>
#'   add_labels(rid = "Rater ID", sid = "Stimulus ID")
#' View(df)
add_labels <- function(.data, ...) {
  cols <- list(...)
  
  for (col in names(cols)) {
    if (col %in% names(.data))
      attr(.data[[col]], "label") <- cols[[col]]
  }
  
  return(.data)
}



#' Recode a categorical column
#'
#' @param .data the data frame
#' @param .col the column to recode
#' @param .newcol the name of the recoded column (defaults to col.c)
#' @param ... coding for categorical column
#'
#' @return data frame with new fixed effects columns
#' @export
#'
#' @examples
#' add_random(subj = 4, item = 4) %>%
#'   add_between("subj", cond = c("cntl", "test")) %>%
#'   add_recode("cond", "cond.t", cntl = 0, test = 1)
add_recode <- function(.data, .col, .newcol = paste0(col, ".c"), ...) {
  .data[.newcol] <- list(.x = .data[[.col]]) %>%
      c(list(...)) %>%
      do.call(dplyr::recode, .)

  .data
}

#' Add random factors to a data structure
#'
#' @param .data the data frame
#' @param ... the new random factor column name and the number of values of the random factor (if crossed) or the n per group (if nested); can be a vector of n per group if nested
#' @param .nested_in the column(s) to nest in (if NULL, the factor is crossed with all columns)
#'
#' @return a data frame
#' @export
#'
#' @examples
#' # start a data frame
#' data1 <- add_random(school = 3)
#' # nest classes in schools (2 classes per school)
#' data2 <- add_random(data1, class = 2, .nested_in = "school")
#' # nest pupils in each class (different n per class)
#' data3 <- add_random(data2, pupil = c(20, 24, 23, 21, 25, 24), .nested_in = "class")
#' # cross each pupil with 10 questions
#' data4 <- add_random(data3, question = 10)
#' 
#' # compare nesting in 2 different factors
#' data <- add_random(A = 2, B = 2)
#' add_random(data, C = 2, .nested_in = "A")
#' add_random(data, C = 2, .nested_in = "B")
#' 
#' # specify item names
#' add_random(school = c("Hyndland Primary", "Hyndland Secondary")) %>%
#'   add_random(class = list(paste0("P", 1:7),
#'                           paste0("S", 1:6)),
#'              .nested_in = "school")
add_random <- function(.data = NULL, ..., .nested_in = NULL) {
  grps <- list(...)
  
  # set design
  design <- attr(.data, "design") %||% list()
  design$id <- design$id %||% list()
  for (x in names(grps)) design$id[[x]] <- x
  design$sep <- design$sep %||% faux_options("sep")

  if (is.null(.nested_in)) {
    # create IDs
    ids <- mapply(function(grp, nm) {
      if (length(grp) == 1 && is.numeric(grp)) {
        make_id(n = grp, prefix = nm)
      } else {
        grp
      }
    }, grps, names(grps), SIMPLIFY = FALSE)
    ranfacs <- do.call(tidyr::crossing, ids)
    .mydata <- .data # stops rlang_data_pronoun warning
    new_data <- tidyr::crossing(.mydata, ranfacs)
    
    if (length(ids) == 1) design$n <- length(ids[[1]])
  } else {
    if (length(grps) > 1) {
      stop("You can only add 1 nested random factor at a time")
    }
    name <- names(grps)[[1]]
    n <- grps[[1]]
    ingrps <- unique(.data[.nested_in])
    if (length(n) == 1) n <- rep(n, nrow(ingrps))
    if (length(n) != nrow(ingrps)) {
      stop("n must be a single integer or a vector ", 
           "with the same length as the number of unique values in ", 
           .nested_in)
    }
    
    if (is.list(n)) {
      all_ids <- unlist(n)
      n <- sapply(n, length)
    } else {
      all_ids <- make_id(sum(n), prefix = names(grps)[[1]])
    }
    
    ids <- data.frame(
      .row = rep(1:nrow(ingrps), times = n),
      y = all_ids
    )
    names(ids)[2] <- name
    ingrps[".row"] <- 1:nrow(ingrps)
    newdat <- dplyr::left_join(ingrps, ids, by = ".row")
    newdat[".row"] <- NULL
    
    new_data <- dplyr::right_join(.data, newdat, by = .nested_in, 
                                  relationship = "many-to-many")
  }
  
  attr(new_data, "design") <- design

  new_data
}
  
#' Add between factors
#'
#' @param .data the data frame
#' @param .by the grouping column (groups by row if NULL)
#' @param ... the names and levels of the new factors
#' @param .shuffle whether to assign cells randomly or in "order"
#' @param .prob probability of each level, equal if NULL
#'
#' @return data frame
#' @export
#'
#' @examples
#' add_random(subj = 4, item = 2) %>%
#'   add_between("subj", condition = c("cntl", "test")) %>%
#'   add_between("item", version = c("A", "B"))
add_between <- function(.data, .by = NULL, ..., .shuffle = FALSE, .prob = NULL) {
  between <- lapply(list(...), function(b) {
    l <- as.list(b)
    names(l) <- names(l) %||% b
    l
  })
  
  design <- attr(.data, "design") %||% list()
  design$between <- design$between %||% list()
  design$between <- c(design$between, between)
  
  if (is.null(.by)) {
    .by <- names(.data)
    grps <- .data
  } else {
    grps <- unique(.data[.by])
  }
  
  if(isTRUE(.shuffle)) grps <- grps[sample(1:nrow(grps)), ]
  
  if (is.null(.prob)) {
    # equal probability for each level
    # return as equal combos as possible 
    vars <- lapply(between, names) %>%
      mapply(factor_char, ., SIMPLIFY = FALSE) %>%
      do.call(tidyr::crossing, .)
      
    for (v in names(vars)) {
      grps[v] <- rep_len(vars[[v]], nrow(grps))
    }
  } else {
    # set prob for each level
    vars <- lapply(between, names) %>% 
      mapply(factor_char, ., SIMPLIFY = FALSE)
    exact_prob <- (sum(unlist(.prob)) == nrow(grps))
    crossed_vars <- do.call(tidyr::crossing, vars)
    
    if (exact_prob && nrow(crossed_vars) == length(.prob)) {
      grps <- crossed_vars %>%
        lapply(rep, times = .prob) %>%
        as.data.frame() %>%
        cbind(grps, .)
    } else {
      warn <- FALSE
      for (v in names(vars)) {
        p <- if (is.na(.prob[v]) || is.null(.prob[[v]])) unlist(.prob) else .prob[[v]]
        p <- rep_len(p, length(vars[[v]]))
        
        if (sum(p) == nrow(grps)) {
          if (!isTRUE(.shuffle) && length(vars) > 1) warn <- TRUE
          grps[v] <- rep(vars[[v]], p)
        } else {
          # randomly sample
          grps[v] <- sample(vars[[v]], nrow(grps), T, prob = p)
        }
      }
      
      if (warn) {
        warning("Allocation can be confounded with exact probabilities and no shuffling. Alternatively, you can specify an exact probability for each cell, e.g.:\n    .prob = c(A1_B1 = 10, A1_B2 = 20, A2_B1 = 30, A2_B2 = 40)")
      }
      
    }
  }
  
  df <- dplyr::left_join(.data, grps, by = .by)
  attr(df, "design") <- design
  
  return(df)
}

#' Add within factors
#'
#' @param .data the data frame
#' @param .by the grouping column (groups by row if NULL)
#' @param ... the names and levels of the new factors
#'
#' @return data frame
#' @export
#'
#' @examples
#' add_random(subj = 2, item =  2) %>%
#'   add_within("subj", time = c("pre", "post"))
add_within <- function(.data, .by = NULL, ...) {
  within <- lapply(list(...), function(w) {
    l <- as.list(w)
    names(l) <- names(l) %||% w
    l
  })
  
  design <- attr(.data, "design") %||% list()
  design$within <- design$within %||% list()
  design$within <- c(design$within, within)
  
  if (is.null(.by)) {
    .by <- names(.data)
    grps <- .data
  } else {
    grps <- unique(.data[.by])
  }
  
  # make vars factors, keep original order
  vars <- lapply(within, names) %>% 
    mapply(factor_char, ., SIMPLIFY = FALSE)
  
  newdat <- c(list(grps), vars) %>%
    do.call(tidyr::crossing, .)
  
  df <- dplyr::left_join(.data, newdat, by = .by, 
                   relationship = "many-to-many")
  
  attr(df, "design") <- design
  
  return(df)
}

# convert only character vectors to factors
factor_char <- function(x) {
  # check if character values should be converted
  if (is.character(x)) {
    if (all(x %in% c("T", "F"))) {
      x <- as.logical(x)
    } else if (all(x %in% suppressWarnings(as.numeric(x)))) {
      x <- as.numeric(x)
    } else if (all(x %in% as.logical(x))) {
      x <- as.logical(x)
    } 
  }
  
  if (is.character(x)) {
    factor(x, x)
  } else {
    x
  }
}
