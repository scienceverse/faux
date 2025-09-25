#' Convert uniform to normal
#'
#' Convert a uniform distribution to a normal (gaussian) distribution with specified mu and sd
#' 
#' @param x the uniformly distributed vector
#' @param mu the mean of the normal distribution to return
#' @param sd the SD of the normal distribution to return
#' @param min the minimum possible value of x (calculated from x if not given)
#' @param max the maximum possible value of x (calculated from x if not given)
#'
#' @return a vector with a gaussian distribution
#' @export
#'
#' @examples
#' 
#' x <- runif(10000)
#' y <- unif2norm(x)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
unif2norm <- function(x, mu = 0, sd = 1, min = NULL, max = NULL) {
  # tol prevents min and max values returning as -Inf and Inf
  tol <- 1/length(x)
  if (is.null(min)) {
    min <- min(x, na.rm = TRUE) - tol
    message("min was not set, so guessed as ", min)
  }
  if (is.null(max)) {
    max <- max(x, na.rm = TRUE) + tol
    message("max was not set, so guessed as ", max)
  }
  
  p <- stats::punif(x, min, max)
  stats::qnorm(p, mu, sd)
}

#' Convert binomial to normal
#'
#' Convert a binomial distribution to a normal (gaussian) distribution with specified mu and sd
#' 
#' @param x the binomially distributed vector
#' @param mu the mean of the normal distribution to return
#' @param sd the SD of the normal distribution to return
#' @param size number of trials (set to max value of x if not specified)
#' @param prob the probability of success on each trial (set to mean probability if not specified)
#'
#' @return a vector with a gaussian distribution
#' @export
#'
#' @examples
#' 
#' x <- rbinom(10000, 20, 0.75)
#' y <- binom2norm(x, 0, 1, 20, 0.75)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
binom2norm <- function(x, mu = 0, sd = 1, size = NULL, prob = NULL) {
  if (!all(as.integer(x) == x, na.rm = TRUE)) stop("all values in x must be integers or NA")
  
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  if (is.null(size)) {
    size <- maxx
    message("size was not set, so guessed as ", size)
  }
  if (is.null(prob)) {
    prob <- mean(x, na.rm = TRUE)/size
    message("prob was not set, so guessed as ", prob)
  }
  
  if (size < maxx) stop("size cannot be smaller than the largest value")
  if (size < 2) stop("size cannot be smaller than 2")
  if (minx < 0) stop("the smallest possible value in a binomial distribution is 0")
  if (prob <0 || prob>1) stop("prob must be between 0 and 1")
  
  # replace infinite values (where x_i == size)
  a <- stats::pbinom((size-2):(size-1), size, prob) %>%
    stats::qnorm(mu, sd)
  replace_inf <- a[2] + (a[2]-a[1])
  
  p <- stats::pbinom(x, size, prob)
  x2 <- stats::qnorm(p, mu, sd)
  x2[x2==Inf] <- replace_inf
  
  x2
}

#' Convert negative binomial to normal
#'
#' Convert a negative binomial distribution to a normal (gaussian) distribution with specified mu and sd
#' 
#' @param x the negative binomially distributed vector
#' @param mu the mean of the normal distribution to return
#' @param sd the SD of the normal distribution to return
#' @param size number of trials (set to max value of x if not specified)
#' @param prob the probability of success on each trial (set to mean probability if not specified)
#'
#' @return a vector with a gaussian distribution
#' @export
#'
#' @examples
#' 
#' x <- rnbinom(10000, 20, 0.75)
#' y <- nbinom2norm(x, 0, 1, 20, 0.75)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
nbinom2norm <- function(x, mu = 0, sd = 1, size = NULL, prob = NULL) {
  if (!all(as.integer(x) == x, na.rm = TRUE)) stop("all values in x must be integers or NA")
  
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  if (is.null(size)) {
    size <- maxx
    message("size was not set, so guessed as ", size)
  }
  if (is.null(prob)) {
    prob <- mean(x, na.rm = TRUE) / size
    message("prob was not set, so guessed as ", prob)
  }
  
  #if (size < maxx) stop("size cannot be smaller than the largest value")
  if (size < 0) stop("size cannot be smaller than 0")
  if (minx < 0) stop("the smallest possible value in a negative binomial distribution is 0")
  if (prob <0 || prob>1) stop("prob must be between 0 and 1")
  
  # replace infinite values (where x_i == size)
  a <- stats::pnbinom((size-2):(size-1), size, prob) %>%
    stats::qnorm(mu, sd)
  replace_inf <- a[2] + (a[2]-a[1])
  
  p <- stats::pnbinom(x, size, prob)
  x2 <- stats::qnorm(p, mu, sd)
  x2[x2==Inf] <- replace_inf
  
  x2
}

#' Convert gamma to normal
#'
#' @param x the gamma distributed vector
#' @param mu the mean of the normal distribution to convert to
#' @param sd the SD of the normal distribution to convert to
#' @param shape gamma distribution parameter (must be positive)
#' @param scale gamma distribution parameter (must be positive)
#' @param rate	an alternative way to specify the scale
#'
#' @return a vector with a normal distribution
#' @export
#'
#' @examples
#' 
#' x <- rgamma(10000, 2)
#' y <- gamma2norm(x)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
gamma2norm <- function(x, mu = 0, sd = 1, shape = NULL, rate = 1, scale = 1/rate) {
  if (scale != 1) rate <- 1/scale
  if (is.null(shape)) {
    shape = mean(x) * rate
    message("shape was not set, so guessed as ", shape)
  }
  p <- stats::pgamma(x, shape, rate = rate)
  stats::qnorm(p, mu, sd)
}

#' Convert beta to normal
#'
#' @param x the gamma distributed vector
#' @param mu the mean of the normal distribution to convert to
#' @param sd the SD of the normal distribution to convert to
#' @param shape1,shape2 non-negative parameters of the beta distribution
#' @param ... further arguments to pass to pbeta (e.g., ncp)
#'
#' @return a vector with a normal distribution
#' @export
#'
#' @examples
#' 
#' x <- rbeta(10000, 2, 3)
#' y <- beta2norm(x)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
beta2norm <- function(x, mu = 0, sd = 1, shape1 = NULL, shape2 = NULL, ...) {
  xmu <- mean(x, na.rm = TRUE)
  xsd <- stats::sd(x, na.rm = TRUE)
  if (is.null(shape1)) {
    shape1 = ((1 - xmu) / xsd^2 - 1 / xmu) * xmu^2
    message("shape1 was not set, so guessed as ", shape1)
  }
  if (is.null(shape2)) {
    shape2 <- shape1 * (1 / xmu - 1)
    message("shape2 was not set, so guessed as ", shape2)
  }
  
  p <- stats::pbeta(x, shape1, shape2, ...)
  stats::qnorm(p, mu, sd)
}

#' Convert normal to poisson
#'
#' @param x the normally distributed vector
#' @param lambda the mean of the distribution to return
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a poisson distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2pois(x, 2)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2pois <- function(x, lambda, mu = mean(x), sd = stats::sd(x)) {
  p <- stats::pnorm(x, mu, sd)
  stats::qpois(p, lambda)
}


#' Convert normal to beta
#'
#' @param x the normally distributed vector
#' @param shape1,shape2 non-negative parameters of the distribution to return
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#' @param ... further arguments to pass to qbeta (e.g., ncp)
#'
#' @return a vector with a beta distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2beta(x, 1, 3)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2beta <- function(x, shape1, shape2, mu = mean(x), sd = stats::sd(x), ...) {
  p <- stats::pnorm(x, mu, sd)
  stats::qbeta(p, shape1, shape2, ...)
}


#' Convert normal to gamma
#'
#' @param x the normally distributed vector
#' @param shape gamma distribution parameter (must be positive)
#' @param scale gamma distribution parameter (must be positive)
#' @param rate	an alternative way to specify the scale
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a gamma distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2gamma(x, shape = 2)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2gamma <- function(x, shape, rate = 1, scale = 1/rate, 
                       mu = mean(x), sd = stats::sd(x)) {
  p <- stats::pnorm(x, mu, sd)
  if (rate == 1 && scale != 1) rate <- 1/scale
  stats::qgamma(p, shape, rate = rate)
}


#' Convert normal to binomial
#'
#' @param x the normally distributed vector
#' @param size number of trials (0 or more)
#' @param prob the probability of success on each trial (0 to 1)
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a binomial distribution
#' @export
#'
#' @examples
#' x <- rnorm(10000)
#' y <- norm2binom(x)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2binom <- function(x, size = 1, prob = 0.5, mu = mean(x), sd = stats::sd(x)) {
  p <- stats::pnorm(x, mu, sd)
  stats::qbinom(p, size, prob)
}

#' Convert normal to negative binomial
#' 
#' See the help for `qnbinom()` for further info about prob versus mu parameter specification. Thanks for the suggested code, David Hugh-Jones!
#' 
#' @param x the normally distributed vector
#' @param size target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution). (size > 0)
#' @param prob the probability of success on each trial (0 to 1)
#' @param mu alternative parametrization via mean (only specify one of prob or mu)
#' @param lower.tail logical; if TRUE (default), probabilities are P[$X <= x$], otherwise, P[$X > x$]
#' @param log.p logical; if TRUE, probabilities p are given as log(p)
#' @param x_mu the mean of x (calculated from x if not given)
#' @param x_sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a negative binomial distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2nbinom(x, 1, prob = 0.5)
#' z <- norm2nbinom(x, 1, mu = 1)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2nbinom <- function (x, size, prob, mu, lower.tail = TRUE, log.p = FALSE, x_mu = mean(x), x_sd = stats::sd(x)) {
  # https://github.com/scienceverse/faux/issues/105
  
  p <- stats::pnorm(x, x_mu, x_sd)
  stats::qnbinom(p, size, prob, mu,
                 lower.tail = lower.tail, 
                 log.p = log.p)
}


#' Convert normal to uniform
#' 
#' Convert a normal (gaussian) distribution to a uniform distribution with specified minimum and maximum
#'
#' @param x the normally distributed vector
#' @param min the minimum of the uniform distribution to return
#' @param max the maximum of the uniform distribution to return
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a uniform distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2unif(x)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2unif <- function(x, min = 0, max = 1, mu = mean(x), sd = stats::sd(x)) {
  p <- stats::pnorm(x, mu, sd)
  stats::qunif(p, min, max)
}

#' Convert normal to normal
#'
#' Convert a normal distribution to a normal (gaussian) distribution with specified mu and sd
#' 
#' @param x the uniformly distributed vector
#' @param mu the mean of the normal distribution to return
#' @param sd the SD of the normal distribution to return
#' @param x_mu the mean of x (calculated from x if not given)
#' @param x_sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a gaussian distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2norm(x, 100, 10)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2norm <- function(x, mu = 0, sd = 1, x_mu = mean(x), x_sd = stats::sd(x)) {
  p <- stats::pnorm(x, x_mu, x_sd)
  stats::qnorm(p, mean = mu, sd = sd)
}

#' Convert normal to truncated normal
#' 
#' Convert a normal (gaussian) distribution to a truncated normal distribution with specified minimum and maximum
#'
#' @param x the normally distributed vector
#' @param min the minimum of the truncated distribution to return
#' @param max the maximum of the truncated distribution to return
#' @param mu the mean of the distribution to return (calculated from x if not given)
#' @param sd the SD of the distribution to return (calculated from x if not given)
#' @param x_mu the mean of x (calculated from x if not given)
#' @param x_sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a uniform distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2trunc(x, 1, 7, 3.5, 2)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2trunc <- function(x, min = -Inf, max = Inf, 
                       mu = mean(x), sd = stats::sd(x), 
                       x_mu = mean(x), x_sd = stats::sd(x)) {
  p <- stats::pnorm(x, x_mu, x_sd)
  truncnorm::qtruncnorm(p, a = min, b = max, mean = mu, sd = sd)
}

#' Convert truncated normal to normal
#' 
#' Convert a truncated normal distribution to a normal (gaussian) distribution
#'
#' @param x the truncated normally distributed vector
#' @param min the minimum of the truncated distribution (calculated from x if not given)
#' @param max the maximum of the truncated distribution (calculated from x if not given)
#' @param mu the mean of the distribution to return (calculated from x if not given)
#' @param sd the SD of the distribution to return (calculated from x if not given)
#'
#' @return a vector with a uniform distribution
#' @export
#'
#' @examples
#' 
#' x <- truncnorm::rtruncnorm(10000, 1, 7, 3.5, 2)
#' y <- trunc2norm(x, 1, 7)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
trunc2norm <- function(x, min = NULL, max = NULL, 
                       mu = mean(x), sd = stats::sd(x)) {
  n <- length(x)
  
  # if not specified, set min and max 
  if (is.null(min)) {
    #min <- mu - (1.5*sd + 0.22*sd*log2(n))
    min <- mu - 3*sd
    message("min was not set, so guessed as ", min, 
            " (min(x) = ", min(x), ")")
  }
  if (is.null(max)) {
    #max <- mu + (1.5*sd + 0.22*sd*log2(n))
    max <- mu + 3*sd
    message("max was not set, so guessed as ", max, 
            " (max(x) = ", max(x), ")")
  }
  
  # make sure min and max encompass data
  if (min > min(x)) {
    min <- min(x) - 0.01*sd
    warning("min was > min(x), so min was set to ", min)
  }
  if (max < max(x)) {
    max <- max(x) + 0.01*sd
    warning("max was < max(x), so max was set to ", max)
  }
  
  p <- truncnorm::ptruncnorm(x, a = min, b = max, mean = mu, sd = sd)
  stats::qnorm(p, mean = mu, sd = sd)
}

#' Convert normal to likert
#'
#' @param x the normally distributed vector
#' @param prob a vector of probabilities or counts; if named, the output is a factor
#' @param labels a vector of values, defaults to names(prob) or 1:length(prob), if numeric, the output is numeric
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#'
#' @return a vector with the specified distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2likert(x, c(.1, .2, .35, .2, .1, .05))
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
#' y <- norm2likert(x, c(40, 30, 20, 10))
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
norm2likert <- function(x, prob, labels = names(prob), mu = mean(x), sd = stats::sd(x)) {
  labels <- labels %||% 1:length(prob)
  p <- stats::pnorm(x, mu, sd)
  qlikert(p, prob, labels)
}

#' Likert density function
#'
#' @param x the likert distributed vector
#' @param prob a vector of probabilities or counts; if named, the output is a factor
#' @param labels a vector of values, defaults to names(prob) or 1:length(prob), if numeric, the output is numeric
#'
#' @return a vector of the densities
#' @export
#'
#' @examples
#' x <- 1:5
#' prob <- c(.1, .2, .4, .2, .1)
#' dlikert(x, prob)
#' 
#' x <- c("A", "C", "B", "B")
#' prob <- c(A = 10, B = 20, C = 30)
#' dlikert(x, prob)
#' 
#' # specify labels if prob not named and not 1:length(prob)
#' labels <- -2:2 
#' x <- sample(labels, 10, replace = TRUE)
#' prob <- rep(1, length(labels)) # uniform probability
#' dlikert(x, prob, labels)
dlikert <- function(x, prob, labels = names(prob)) {
  labels <- labels %||% 1:length(prob)
  dtrans <- stats::setNames(prob/sum(prob), labels)
  dtrans[as.character(x)]
}

#' Likert distribution function
#'
#' @param q the vector of quantiles
#' @param prob a vector of probabilities or counts; if named, the output is a factor
#' @param labels a vector of values, defaults to names(prob) or 1:length(prob), if numeric, the output is numeric
#'
#' @return a vector of the densities
#' @export
#'
#' @examples
#' q <- 1:5
#' prob <- c(.1, .2, .4, .2, .1)
#' plikert(q, prob)
#' 
#' q <- c("A", "C", "B", "B")
#' prob <- c(A = 10, B = 20, C = 30)
#' plikert(q, prob)
#' 
#' # specify labels if prob not named and not 1:length(prob)
#' labels <- -2:2 
#' q <- labels
#' prob <- rep(1, length(labels)) # uniform probability
#' plikert(q, prob, labels)
plikert <- function(q, prob, labels = names(prob)) {
  labels <- labels %||% 1:length(prob)
  ptrans <- stats::setNames(cumsum(prob/sum(prob)), labels)
  ptrans[as.character(q)]
}

#' Likert quantile function
#'
#' @param p the vector of probabilities
#' @param prob a vector of probabilities or counts; if named, the output is a factor
#' @param labels a vector of values, defaults to names(prob) or 1:length(prob), if numeric, the output is numeric
#'
#' @return a vector of the quantiles
#' @export
#'
#' @examples
#' p <- seq(0, 1, .1)
#' prob <- c(.1, .2, .4, .2, .1)
#' qlikert(p, prob)
#' 
#' p <- seq(0, 1, .1)
#' prob <- c(A = 10, B = 20, C = 30)
#' qlikert(p, prob)
#' 
#' # specify labels if prob not named and not 1:length(prob)
#' labels <- -2:2 
#' p <- seq(0, 1, .1)
#' prob <- rep(1, length(labels)) # uniform probability
#' qlikert(p, prob, labels)
qlikert <- function(p, prob, labels = names(prob)) {
  labels <- labels %||% 1:length(prob)
  ptrans <- stats::setNames(cumsum(prob/sum(prob)), labels)
  q <- lapply(p, function(psub) {
      greater <- psub > ptrans
      # deal with floating point inequalities where x != x
      noneq <- sapply(ptrans, all.equal, current = psub) != TRUE
      greater & noneq
    }) %>%
    sapply(sum) %>%
    sapply(`+`, 1) %>%
    `[`(labels, .)
  
  if (!is.numeric(labels)) factor(q, labels) else q
}

#' Random Likert distribution
#'
#' @param n the number of observations
#' @param prob a vector of probabilities or counts; if named, the output is a factor
#' @param labels a vector of values, defaults to names(prob) or 1:length(prob), if numeric, the output is numeric
#'
#' @return a vector sampled from a likert distribution with the specified parameters
#' @export
#'
#' @examples
#' # no names or labels returns integer vector of values 1:length(prob)
#' prob <- c(.1, .2, .4, .2, .1)
#' rlikert(10, prob)
#' 
#' # named prob returns factor 
#' prob <- c(A = 10, B = 20, C = 30)
#' rlikert(10, prob)
#' 
#' # specify labels if prob not named and not 1:length(prob)
#' labels <- -2:2 
#' prob <- rep(1, length(labels)) # uniform probability
#' rlikert(10, prob, labels)
rlikert <- function(n, prob, labels = names(prob)) {
  labels <- labels %||% 1:length(prob)
  if (!is.numeric(labels)) labels <- factor(labels, labels, labels)
  sample(labels, n, replace = TRUE, prob = prob)
}



#' Standardized Alpha to Average R
#'
#' @param std_alpha The standarized alpha 
#' @param n The number of items
#'
#' @return The average inter-item correlation
#' @export
#'
#' @examples
#' std_alpha2average_r(.8, 10)
std_alpha2average_r <- function(std_alpha, n) {
  sumR <- -n / ((std_alpha / (n/(n - 1))) - 1)
  (sumR - n)/(n * (n - 1))
}

#' Average r to Random Intercept SD
#'
#' @param average_r The average inter-item correlation
#' @param sigma Total error variance
#'
#' @return The standard deviation of the random intercept
#' @export
average_r2tau_0 <- function(average_r, sigma) {
  sqrt((average_r * sigma^2) / (1 - average_r))
}
