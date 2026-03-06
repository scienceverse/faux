#' Convert r for NORTA
#' 
#' Given a target r-value, returns the correlation you need to induce in a bivariate normal distribution to have the target correlation after converting distributions. 
#' 
#' See [Distributions][stats::Distributions] for distributions and their various arguments to specify in params1 and params2.
#'
#' @param target_r The target correlation
#' @param dist1 The target distribution function for variable 1 (e.g., norm, binom, gamma, truncnorm)
#' @param dist2 The target distribution function for variable 2
#' @param params1 Arguments to pass to the functions for distribution 1
#' @param params2 Arguments to pass to the functions for distribution 2
#' @param tol Tolerance for optimise function
#'
#' @return r-value to induce in the bivariate normal variables
#' @export
#'
#' @examples
#' convert_r(target_r = 0.5, 
#'           dist1 = "norm", 
#'           dist2 = "binom", 
#'           params1 = list(mean = 100, sd = 10),
#'           params2 = list(size = 1, prob = 0.5))
convert_r <- function(target_r = 0,
                      dist1 = "norm", 
                      dist2 = "norm",
                      params1 = list(), 
                      params2 = list(),
                      tol = .01) {
  if (target_r == 0) return(0)
  if (dist1 == "norm" & dist2 == "norm") return(target_r)
  params1 <- as.list(params1)
  params2 <- as.list(params2)
  
  if ("cauchy" %in% c(dist1, dist2)) {
    stop("The cauchy distribution has undefined variance, so cannot use the NORTA method.")
  }

  # check if target_r is possible
  bounds <- fh_bounds(dist1, dist2, params1, params2)
  
  if (target_r > bounds$max) {
    warning("Maximum target_r is ", round(bounds$max, 3))
    return(NA)
  }
  if (target_r < bounds$min) {
    warning("Minimum target_r is ", round(bounds$min, 3))
    return(NA)
  }
  
  # define function
  qfunc1 <- distfuncs(dist1)$q
  qfunc2 <- distfuncs(dist2)$q
  
  f <- function(r) {
    # simulate bivariate normal with specified r
    mvn <- rnorm_multi(5e4, 2, r = r, empirical = TRUE)
    params1$p <- stats::pnorm(mvn$X1, 0, 1)
    params2$p <- stats::pnorm(mvn$X2, 0, 1)
    
    # convert dist1 & dist2 
    # make numeric in case likert
    X1 <- do.call(qfunc1, params1) %>% as.numeric()
    X2 <- do.call(qfunc2, params2) %>% as.numeric()
    
    # check new correlation
    conv_r <- cor(X1, X2)
    
    # compare to target r (goal to minimise this)
    abs(conv_r - target_r)
  }
  
  # optimise to find converted r
  if (target_r > 0) min_r <- 0; max_r <- 0.99
  if (target_r < 0) max_r <- 0; min_r <- -.99
  
  # set seed and reinstate system seed after simulation
  # makes sure the results are always the same
  # CRAN says I can't do this :(
  # sysSeed <- .GlobalEnv$.Random.seed
  # on.exit({
  #   if (!is.null(sysSeed)) {
  #     .GlobalEnv$.Random.seed <- sysSeed
  #   } else {
  #     rm(".Random.seed", envir = .GlobalEnv)
  #   }
  # })
  # set.seed(8675309, kind = "Mersenne-Twister", normal.kind = "Inversion")
  
  opt <- stats::optimise(f, interval = c(min_r, max_r), tol = tol)
  
  # check if found
  if (abs(opt$objective) > .01) {
    warning("The target_r could not be matched any closer than ",
            round(opt$objective, 3))
  }
  
  round(opt$minimum, 3)
}


#' Multiple correlated distributions
#'
#' @param n the number of samples required
#' @param dist A named vector of the distributions of each variable
#' @param params A list of lists of the arguments to pass to each distribution function
#' @param r the correlations among the variables (can be a single number, vars$\times$vars matrix, vars$\times$vars vector, or a vars$\times$(vars-1)/2 vector)
#' @param empirical logical. If true, params specify the sample parameters, not the population parameters 
#' @param as.matrix logical. If true, returns a matrix
#'
#' @return a tbl of vars vectors
#' @export
#'
#' @examples
#' dist <- c(A = "norm", 
#'           B = "pois", 
#'           C = "binom")
#' params <- list(A = list(mean = 100, sd = 10),
#'                B = list(lambda = 5),
#'                C = list(size = 10, prob = 0.5))
#' x <- rmulti(100, dist, params, c(0.2, 0.4, 0.6), empirical = TRUE)
#' sample_params(x)
rmulti <- function(n = 100, 
                   dist = c(A = "norm", B = "norm"), 
                   params = list(),
                   r = 0,
                   empirical = FALSE, 
                   as.matrix = FALSE) {
  vars <- length(dist)
  
  if (vars == 1) {
    # get rfunc
    rfunc <- distfuncs(dist[[1]])$r
    params[[1]]$n <- n
    values <- do.call(rfunc, params[[1]])
    return(values)
  }

  if (vars < 2) stop("You must specify at least 2 variables in `dist`")
  varnames <- names(dist) %||% LETTERS[1:vars]
  
  # get all possible pairs of variables
  v <- factor(varnames, levels = varnames)
  all <- expand.grid(v1 = v, v2 = v)
  a_less <- as.integer(all$v1) < as.integer(all$v2)
  pairs <- all[a_less, ]
  pairs <- pairs[order(pairs$v1, pairs$v2), ] # put in order (thanks @yann1cks!)
  
  # add correlations
  cormat <- cormat(r, vars)
  pairs$r <- cormat[lower.tri(cormat)]
  
  # add distributions
  pairs$dist1 <- dist[pairs$v1]
  pairs$dist2 <- dist[pairs$v2]
  
  # add params
  if (length(params) == 0) {
    params <- rep(list(list()), length.out = vars)
  }
  if (is.null(names(params))) names(params) <- varnames
  pairs$params1 <- params[pairs$v1]
  pairs$params2 <- params[pairs$v2]
  
  # calculate adjusted r-values
  suppressWarnings({ # take care of warnings later
    pairs$adj_r <- mapply(convert_r, pairs$r, 
                          pairs$dist1, pairs$dist2, 
                          pairs$params1, pairs$params2)
  })
  
  # quit if any params are impossible
  if (any(is.na(pairs$adj_r))) {
    impossible_r <- which(is.na(pairs$adj_r))
    imp_pairs <- lapply(impossible_r, function(i) {
      b <- fh_bounds(dist1 = pairs$dist1[[i]], 
                    dist2 = pairs$dist2[[i]],
                    params1 = params[[pairs$v1[[i]]]],
                    params2 = params[[pairs$v2[[i]]]])
      paste0(pairs$v1[i], "&", pairs$v2[i], " (", 
             round(b$min, 3), " to ", round(b$max, 3), ")")
    })
  
    stop("Some of the correlations are not possible:\n  *  ", 
         paste(imp_pairs, collapse = "\n  *  "))
  }
  
  adj_norm <- rnorm_multi(n, vars, 0, 1, 
                          r = pairs$adj_r, 
                          varnames = varnames, 
                          empirical = empirical,
                          as.matrix = as.matrix)
  
  for (i in 1:vars) {
    #get params
    qparam <- as.list(params[[varnames[i]]])
    x <- adj_norm[, i]
    qparam$p <- stats::pnorm(x, mean = 0, sd = 1)
    
    # get quantile function
    qfunc <- distfuncs(dist[i])$q
    
    adj_norm[, i] <- tryCatch({
      do.call(qfunc, qparam)
    }, error = function(e) {
      stop("Check the `params` argument for variable '", varnames[i],"'.\n", e)
    })
  }
  
  adj_norm
}


#' Get distribution functions
#'
#' @param dist The target distribution function (e.g., norm, binom, gamma, truncnorm, likert). If the distribution isn't definited in the packages stats, truncnorm, or faux, use the format "package::dist".
#'
#' @return a list with the r and q functions
#' @export
#'
#' @examples
#' qfunc <- distfuncs("norm")$q # returns qnorm
#' p <- seq(0.1, 0.9, .1)
#' qfunc(p) == qnorm(p)
#' 
#' rfunc <- distfuncs("norm")$r # returns rnorm
#' rfunc(n = 10, mean = 100, sd = 10)
distfuncs <- function(dist = "norm") {
  # get package
  package <- "stats" # default
  if (dist == "truncnorm") package = "truncnorm"
  if (dist == "likert") package = "faux"
  if (grepl("::", dist)) {
    package <- strsplit(dist, "::")[[1]][[1]]
    dist <- strsplit(dist, "::")[[1]][[2]]
  }
  
  # check if r and q functions exist
  tryCatch({
    rfunc <- utils::getFromNamespace(paste0("r", dist), package)
    qfunc <- utils::getFromNamespace(paste0("q", dist), package)
  }, error = function(e) {
    stop("The probability functions for the '", dist, "' distribution could not be found in the package {", package, "}. Check the `dist` argument.", call. = FALSE)
  })
  
  list(r = rfunc,
       q = qfunc)
}

#' Get Fréchet-Hoefding bounds 
#' 
#' Fréchet-Hoefding bounds are the limits to a correlation between different distributions.
#'
#' @param dist1 The target distribution function for variable 1 (e.g., norm, binom, gamma, truncnorm)
#' @param dist2 The target distribution function for variable 2
#' @param params1 Arguments to pass to the random generation function (e.g., rnorm) for distribution 1
#' @param params2 Arguments to pass to the random generation function (e.g., rnorm) for distribution 2
#'
#' @return a list of the min and max possible values
#' @export
#'
#' @examples
#' fh_bounds(dist1 = "pois", 
#'          dist2 = "unif", 
#'          params1 = list(lambda = 3), 
#'          params2 = list(min = 0, max = 100))
fh_bounds <- function(dist1 = "norm",
                     dist2 = "norm", 
                     params1 = list(),
                     params2 = list()) {
  params1 <- as.list(params1)
  params2 <- as.list(params2)
  
  # get random generation functions
  rfunc1 <- distfuncs(dist1)$r
  rfunc2 <- distfuncs(dist2)$r
  
  # generate target distributions
  params1$n <- 1e6
  D1 <- sort(do.call(rfunc1, params1)) %>% as.numeric()
  params2$n <- 1e6
  D2 <- sort(do.call(rfunc2, params2)) %>% as.numeric()
  
  # return min and max
  list(
    min = cor(D1, rev(D2)),
    max = cor(D1, D2)
  )
}
