#' Multiple correlated normal distributions
#' 
#' Make normally distributed vectors with specified relationships. See [`vignette("rnorm_multi", package = "faux")`](../doc/rnorm_multi.html) for details.
#'
#' @param n the number of samples required
#' @param vars the number of variables to return
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' @param r the correlations among the variables (can be a single number, vars$\times$vars matrix, vars$\times$vars vector, or a vars$\times$(vars-1)/2 vector)
#' @param varnames optional names for the variables (string vector of length vars) defaults if r is a matrix with column names
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param as.matrix logical. If true, returns a matrix
#' @param seed DEPRECATED use set.seed() instead before running this function
#' 
#' @return a tbl of vars vectors
#' 
#' @examples
#' # 4 10-item vectors each correlated r = .5
#' rnorm_multi(10, 4, r = 0.5)
#' 
#' # set r with the upper right triangle
#' b <- rnorm_multi(100, 3, c(0, .5, 1), 1, 
#'                  r = c(0.2, -0.5, 0.5), 
#'                  varnames=c("A", "B", "C"))
#' cor(b)
#' 
#' # set r with a correlation matrix and column names from mu names
#' c <- rnorm_multi(
#'   n = 100, 
#'   mu = c(A = 0, B = 0.5, C = 1),
#'   r = c( 1,   0.2, -0.5, 
#'          0.2, 1,    0.5, 
#'         -0.5, 0.5,  1)
#' )
#' cor(c)
#' 
#' @export

rnorm_multi <- function(n = 100, vars = NULL, mu = 0, sd = 1, r = 0,
                       varnames = NULL, empirical = FALSE, 
                       as.matrix = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() instead")
  #   # reinstate system seed after simulation
  #   gs <- global_seed(); on.exit(global_seed(gs))
  }
  
  # error handling ----
  if ( !is.numeric(n) || n %% 1 > 0 || n < 1 ) {
    stop("n must be an integer > 0")
  }
  
  if (!(empirical  %in% c(TRUE, FALSE))) {
    stop("empirical must be TRUE or FALSE")
  }
  
  # try to guess vars if not set ----
  if (is.null(vars)) {
    if (!is.null(varnames)) {
      vars <- length(varnames)
    } else if (length(mu) > 1) {
      vars <- length(mu)
    } else if (length(sd) > 1) {
      vars <- length(sd)
    } else if (is.matrix(r)) {
      vars <- ncol(r)
    }
    
    if (is.null(vars)) {
      stop("The number of variables (vars) was not explicitly set and can't be guessed from the input.")
    }
  }
  
  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars");
  } else {
    # get rid of names
    #mu <- as.matrix(mu) %>% as.vector()
  }
  
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  } else {
    # get rid of names
    #sd <- as.matrix(sd) %>% as.vector()
  }
  
  if (n == 1 & empirical == TRUE) {
    warning("When n = 1 and empirical = TRUE, returned data are equal to mu")
    mvn <- mu
    cor_mat <- r # for name-checking later
  } else {
    # get data from mvn ----
    cor_mat <- tryCatch(
      { cormat(r, vars) }, 
      error = function(e) {
        stop("The correlation you specified is impossible: ", e$message, call. = FALSE)
      }
    )
                        
    sigma <- (sd %*% t(sd)) * cor_mat
    # tryCatch({
    #   mvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
    # }, error = function(e) {
    #   stop("The correlated variables could not be generated. If empirical = TRUE, try increasing the N or setting empirical = FALSE.")
    # })
    
    err <- "The correlated variables could not be generated."
    if (empirical) err <- paste(err, "Try increasing the N or setting empirical = FALSE.")
    
    # code from MASS:mvrnorm
    p <- length(mu)
    if (!all(dim(sigma) == c(p, p))) stop(err)
    eS <- eigen(sigma, symmetric = TRUE)
    ev <- eS$values
    if (!all(ev >= -1e-06 * abs(ev[1L]))) stop(paste(err))
    X <- matrix(stats::rnorm(p * n), n)
    if (empirical) {
      X <- scale(X, TRUE, FALSE)
      X <- X %*% svd(X, nu = 0)$v
      X <- scale(X, FALSE, TRUE)
    }
    tryCatch({
      X <- drop(mu) + eS$vectors %*% 
        diag(sqrt(pmax(ev, 0)), p) %*%  t(X)
    }, error = function(e) { stop(err) })
    
    mvn <- t(X)
  }
  
  # coerce to matrix if vector when n == 1
  if (n == 1) mvn <- matrix(mvn, nrow = 1)
  
  if (length(varnames) == vars) {
    colnames(mvn) <- varnames
  } else if (!is.null(colnames(cor_mat))) {
    # if r was a matrix with names, use that
    colnames(mvn) <- colnames(cor_mat)
  } else if (!is.null(names(mu))) {
    #use mu names 
    colnames(mvn) <- names(mu)
  } else if (!is.null(names(sd))) {
    #use sd names 
    colnames(mvn) <- names(sd)
  } else {
    colnames(mvn) <- make_id(ncol(mvn), "X")
  }
  
  if (as.matrix == TRUE) mvn else data.frame(mvn, check.names = FALSE)
}

