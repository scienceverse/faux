r <- c(-.5, .2, .5)
set.seed(8675309)

# distfuncs ----
test_that("distfuncs", {
  func <- distfuncs("norm")
  expect_equal(func$q, qnorm)
  expect_equal(func$r, rnorm)
  
  func <- distfuncs("pois")
  expect_equal(func$q, qpois)
  expect_equal(func$r, rpois)
  
  func <- distfuncs("truncnorm")
  expect_equal(func$q, truncnorm::qtruncnorm)
  expect_equal(func$r, truncnorm::rtruncnorm)
  
  ## with :: notation
  func <- distfuncs("stats::norm")
  expect_equal(func$q, qnorm)
  expect_equal(func$r, rnorm)
  
  expect_error( distfuncs("normz") )
  expect_error( distfuncs("nopkg::norm") )
})

# fh_bounds ----
test_that("fh_bounds", {
  set.seed(8675309)
  
  b <- fh_bounds("norm", "unif")
  expect_equal(b$max, 0.977, tol = .005)
  expect_equal(b$min, -0.977, tol = .005)
  
  b <- fh_bounds("norm", "unif", 
                list(mean = 100, sd = 10), 
                list(min = 0, max = 100))
  expect_equal(b$max, 0.977, tol = .005)
  expect_equal(b$min, -0.977, tol = .005)
  
  b <- fh_bounds("pois", "truncnorm", 
                list(lambda = 1), 
                list(a = 0, b = 10, mean = 5, sd = 2))
  expect_equal(b$max, 0.913, tol = .005)
  expect_equal(b$min, -0.914, tol = .005)
  
  b <- fh_bounds("pois", "binom", 
                list(lambda = 3),
                list(size = 1, prob = 0.5))
  expect_equal(b$max, 0.776, tol = .005)
  expect_equal(b$min, -0.776, tol = .005)
})

test_that("fh_bounds variation check", {
  skip("long checking function")
  
  x <- lapply(1:50, function(x) {
    fh_bounds("pois", "binom",  
             list(lambda = 3),
             list(size = 1, prob = 0.5))
  })
  
  hist(sapply(x, `[[`, "min"))
})


# convert_r ----
set.seed(8675309)
test_that("convert_r warnings", {
  # warnings and errors
  expect_warning(convert_r(.95, "binom", params1 = list(size = 1, prob = 0.5)))
  expect_error(convert_r(0.3, "nope1"))
  expect_error(convert_r(0.3, "unif", "nope2"))
})

## norm:norm ----
test_that("convert_r norm:norm", {
  x <- sapply(r, convert_r)
  #plot(r, x)
  expect_equal(r, x)
  
  # norm with changed SD
  x <- sapply(r, convert_r, 
              params1 = list(mean = 10, sd = 5),
              params2 = list(mean = -10, sd = 2))
  #plot(r, x)
  expect_equal(r, x)
})

## norm:binom ----
test_that("convert_r norm:binom", {
  skip("long simulation")
  size <- 2
  prob <- .5
  x <- sapply(r, convert_r, dist2 = "binom",
              params2 = list(size = size, prob = prob))
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    check_dist$X2 <- norm2binom(check_dist$X2, 
                                size = size, 
                                prob = prob, 
                                mu = 0, sd = 1)
    cor(check_dist$X1, check_dist$X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## binom:binom
test_that("convert_r binom:binom", {
  skip("long simulation")
  size1 <- 2
  prob1 <- .5
  size2 <- 3
  prob2 <- 1/3
  x <- sapply(r, convert_r, 
              dist1 = "binom", dist2 = "binom",
              params1 = list(size = size1, prob = prob1),
              params2 = list(size = size2, prob = prob2))
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- norm2binom(check_dist$X1, 
                      size = size1, 
                      prob = prob1, 
                      mu = 0, sd = 1)
    X2 <- norm2binom(check_dist$X2, 
                      size = size2, 
                      prob = prob2, 
                      mu = 0, sd = 1)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## norm:pois ----
test_that("convert_r norm:pois", {
  skip("long simulation")
  lambda <- 1.5
  x <- sapply(r, convert_r, dist2 = "pois",
              params2 = list(lambda = lambda))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- check_dist$X1
    X2 <- norm2pois(check_dist$X2, 
                    lambda = lambda, 
                    mu = 0, sd = 1)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## pois:pois ----
test_that("convert_r pois:pois", {
  skip("long simulation")
  lambda1 <- 1
  lambda2 <- 2
  x <- sapply(r, convert_r, 
              dist1 = "pois",
              dist2 = "pois",
              params1 = list(lambda = lambda1),
              params2 = list(lambda = lambda2))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- norm2pois(check_dist$X1, 
                    lambda = lambda1, 
                    mu = 0, sd = 1)
    X2 <- norm2pois(check_dist$X2, 
                    lambda = lambda2, 
                    mu = 0, sd = 1)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## norm:beta ----
test_that("convert_r norm:beta", {
  skip("long simulation")
  shape1 <- 1.1
  shape2 <- 1.2
  x <- sapply(r, convert_r, dist2 = "beta",
              params2 = list(shape1 = shape1,
                             shape2 = shape2))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- check_dist$X1
    X2 <- norm2beta(check_dist$X2, 
                    shape1 = shape1, 
                    shape2 = shape2,
                    mu = 0, sd = 1)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## norm:gamma ----
test_that("convert_r norm:gamma", {
  skip("long simulation")
  shape <- 1.5
  rate <- 1.2
  x <- sapply(r, convert_r, dist2 = "gamma",
              params2 = list(shape = shape,
                             rate = rate))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- check_dist$X1
    X2 <- norm2gamma(check_dist$X2, 
                    shape = shape,
                    rate = rate,
                    mu = 0, sd = 1)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})


## norm:likert ----
test_that("convert_r norm:likert", {
  skip("long simulation")
  prob <- c(10, 20, 40, 20, 10)
  labels <- LETTERS[1:length(prob)]
  x <- sapply(r, convert_r, dist2 = "likert",
              params2 = list(prob = prob, 
                             labels = labels))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- check_dist$X1
    X2 <- norm2likert(check_dist$X2, 
                      prob = prob,
                      labels = labels,
                      mu = 0, sd = 1) %>% as.numeric()
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

# rmulti ----

test_that("rmulti errors", {
  expect_error( rmulti(n = "A") )
  expect_error( rmulti(dist = c(A = "norm")) )
  expect_error( rmulti(dist = c("norm", "blue")) )
  
  # error when params don't match dist
  badparams <- list(A = c(mu = 10, stdev = 3), 
                    B = c(meh = 0, so = 1))
  expect_error( rmulti(params = badparams) )
})

test_that("rmulti default", {
  set.seed(8675309)
  x <- rmulti()
  expect_equal(nrow(x), 100L)
  expect_equal(names(x), c("A", "B"))
  expect_true(abs(mean(x$A)) < .1)
  expect_true(abs(1 - sd(x$A)) < .1)
  
  set.seed(8675309)
  x2 <- rmulti()
  expect_identical(x$A, x2$A)
  expect_identical(x$B, x2$B)
  
  y <- rmulti()
  expect_false(all(x$A == y$A))
  
  # no names, but right order
  set.seed(8675309)
  nonameparams <- list(A = c(100, 10), 
                   B = c(0, 1))
  x <- rmulti(params = nonameparams)
  expect_true(mean(x$A) > 80)
  expect_true(sd(x$A) > 5)
  expect_true(sd(x$A) < 15)
  expect_true(mean(x$B) < 5)
})

test_that("rmulti", {
  skip("long simulation")
  
  r <- seq(.1, .6, .1)
  dist <- c(A = "norm", B = "binom", C = "beta", D = "pois")
  params <- list(A = list(mean = 10, sd = 5), 
                 B = list(size = 6, prob = 0.5),
                 C = list(shape1 = 2, shape2 = 2),
                 D = list(lambda = 10))
  
  x <- rmulti(n = 100, 
              dist = dist, 
              params = params, 
              r = r, 
              empirical = TRUE)
  
  recov_r <- cor(x)
  diff <- abs(recov_r[lower.tri(recov_r)] - r)
  expect_true(all(diff < .05))
})

test_that("rmulti impossible r", {
  dist <- c(A = "pois", B = "binom")
  params <- list(A = list(lambda = 3), 
                 B = list(size = 1, prob = 0.5))
  r = 0.8
  
  expect_error({
    x <- rmulti(n = 100, 
              dist = dist, 
              params = params, 
              r = r, 
              empirical = TRUE)
  })
})

test_that("rmulti 5", {
  # https://github.com/scienceverse/faux/issues/107
  r <- c(1, 0.1, 0.2, 0.3, 0.4,
         0.1, 1, 0.5, 0.6, 0.7,
         0.2, 0.5, 1, 0.8, 0.1,
         0.3, 0.6, 0.8, 1, 0.2,
         0.4, 0.7, 0.1, 0.2, 1)
  
  # Simulate data. 
  data <- rmulti(
    n = 1000,
    dist = c(A = "norm", B = "norm", C = "norm", D = "norm",  E = "norm"),
    params = list(
      A = list(mean = 1, sd = 1),
      B = list(mean = 2, sd = 2),
      C = list(mean = 3, sd = 3),
      D = list(mean = 4, sd = 4),
      E = list(mean = 5, sd = 5)
    ),
    r = r,
    empirical = TRUE
  )
  p <- get_params(data)
  
  expect_equal(p$A, c(1.0, 0.1, .2, .3, .4))
  expect_equal(p$B, c(.1, 1.0, .5, .6, .7))
  expect_equal(p$C, c(.2, .5, 1.0, .8, .1))
  expect_equal(p$D, c(0.3, 0.6, 0.8, 1, 0.2))
  expect_equal(p$E, c(0.4, 0.7, 0.1, 0.2, 1))
  expect_equal(p$mean, 1:5)
  expect_equal(p$sd, 1:5)
})

test_that("rmulti r", {
  r <- seq(0, .5, .1)
  
  # Simulate data. 
  data <- rmulti(
    n = 1000,
    dist = c(Z = "norm", Y = "norm", X = "norm", W = "norm"),
    params = list(
      W = list(mean = 1, sd = 1),
      X = list(mean = 2, sd = 2),
      Y = list(mean = 3, sd = 3),
      Z = list(mean = 4, sd = 4)
    ),
    r = r,
    empirical = TRUE
  )
  p <- get_params(data)
  
  expect_equal(p$Z, c(1, 0, .1, .2))
  expect_equal(p$Y, c(0, 1, .3, .4))
  expect_equal(p$X, c(.1, .3, 1, .5))
  expect_equal(p$W, c(.2, .4, .5, 1))
  expect_equal(p$mean, 4:1)
  expect_equal(p$sd, 4:1)
})
