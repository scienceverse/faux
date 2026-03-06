#' Sample Parameters from Population Parameters
#'
#' @param n sample size
#' @param mu population mean
#' @param sd population SD
#' @param r population r
#'
#' @return list of sample parameters (mu, sd, r)
#' @export
#'
#' @examples
#' sample_from_pop(10)
sample_from_pop <- function(n = 100, mu = 0, sd = 1, r = 0) {
  # sample r from distribution depending on n and r
  r_sd <- sqrt(1/n) * (1-r^2)
  #sample_r <- stats::rnorm(1, r, r_sd) %>% pmax(-1) %>% pmin(1)
  sample_r <- mapply(stats::rnorm, 1, r, r_sd) %>% pmax(-1) %>% pmin(1)
  
  # sample mu from distribution depending on n and sd
  mu_sd <- sd / sqrt(n)
  #sample_mu <- stats::rnorm(1, mu, mu_sd)
  sample_mu <- mapply(stats::rnorm, 1, mu, mu_sd)
  
  # sample sd from distribution depending on n and sd
  sd_sd <- sd / sqrt(2*n)
  # sample_sd <- stats::rnorm(1, sd, sd_sd)
  sample_sd <- mapply(stats::rnorm, 1, sd, sd_sd)
  
  list(
    mu = sample_mu,
    sd = sample_sd,
    r = sample_r
  )
}



#' Make a normal vector correlated to existing vectors
#'
#' `rnorm_pre` Produces a random normally distributed vector with the specified correlation to one or more existing vectors
#'
#' @param x the existing vector or data table of all vectors
#' @param mu desired mean of returned vector
#' @param sd desired SD of returned vector
#' @param r desired correlation(s) between existing and returned vectors
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param threshold for checking correlation matrix
#' 
#' @return vector
#' @examples
#' v1 <- rnorm(10)
#' v2 <- rnorm_pre(v1, 0, 1, 0.5)
#' cor(v1, v2)
#' 
#' x <- rnorm_multi(50, 2, .5)
#' x$y <- rnorm_pre(x, r = c(0.5, 0.25))
#' cor(x)
#' @export
rnorm_pre <- function (x, mu=0, sd=1, r=0, empirical = FALSE, threshold = 1e-12) {
  # error checking
  if (is.atomic(x)) x <- data.frame(x)
  #if (!is.numeric(x)) stop("x must be numeric")
  if (nrow(x) < 3) stop("x must have length > 2")
  
  n <- nrow(x)
  d <- ncol(x)
  rho <- rep(r, length.out = d)
  
  if (!empirical) {
    sample_params <- sample_from_pop(n, mu, sd, rho)
    mu <- sample_params$mu
    sd <- sample_params$sd
    rho <- sample_params$r
  }
  # https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables
  # answer by https://stats.stackexchange.com/users/919/whuber
  
  # Process the arguments.
  if (is.data.frame(x)) x <- as.matrix(x)
  x <- scale(x) # Makes computations simpler
  y <- stats::rnorm(n)
  
  # Remove the effects of `x` on `y`.
  e <- stats::residuals(stats::lm(y ~ x))
  
  if (d == 1) {
    z <- z <- rho * scale(x)[,1] + sqrt(1 - rho^2) * 
      scale(e)[,1]
  } else {
    # Calculate the coefficient `sigma` of `e` 
    # so that the correlation of `x` with the linear combination 
    # x.dual %*% rho + sigma*e is the desired vector.
    x.dual <- with(svd(x), (n-1)*u %*% diag(ifelse(d > 0, 1/d, 0)) %*% t(v))
    sigma2 <- c((1 - rho %*% stats::cov(x.dual) %*% rho) / stats::var(e))
    
    # Return this linear combination.
    if (sigma2 >= 0) {
      sigma <- sqrt(sigma2) 
      z <- x.dual %*% rho + sigma*e
    } else {
      warning("Correlations are impossible.")
      z <- rep(0, n)
    }
  }
  
  as.vector(mu + sd * z)
}

