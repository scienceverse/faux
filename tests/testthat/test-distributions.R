context("test-distributions")

# error ----
test_that("error", {
  missing_x <- "argument \"x\" is missing, with no default"
  expect_error(norm2unif(), missing_x)
  expect_error(norm2pois(), missing_x)
  expect_error(norm2beta(), missing_x)
  expect_error(norm2binom(), missing_x)
  expect_error(unif2norm(), missing_x)
  expect_error(gamma2norm(), missing_x)
  expect_error(norm2gamma(), missing_x)
  expect_error(binom2norm(), missing_x)
  expect_error(norm2nbinom(), missing_x)
  
  expect_error(norm2pois(1), "argument \"lambda\" is missing, with no default")
  expect_error(norm2beta(1), "argument \"shape1\" is missing, with no default")
  expect_error(norm2beta(1, 1), "argument \"shape2\" is missing, with no default")
  expect_error(norm2gamma(1), "argument \"shape\" is missing, with no default")
  expect_error(norm2nbinom(1), "argument \"size\" is missing, with no default")
  
  expect_error(binom2norm(c(1,2,3.3)), "all values in x must be integers or NA")
  expect_error(binom2norm(c(1,2,-3)), "the smallest possible value in a binomial distribution is 0")
  expect_error(binom2norm(c(1,2,3), size = 2), "size cannot be smaller than the largest value")
  expect_error(binom2norm(c(0, 1), size = 1), "size cannot be smaller than 2")
})

set.seed(1)

n <- 100 # n for each tested vector
reps <- 1 # how many times to sample parameters
mapreps <- 1:50 # how many times to test 1 set of parameters
tol <- 0.05 # tolerance for comparing sampled parameter values to simulated values

# uniform ----
test_that("uniform", {
  # convert between normal and uniform and back to check 
  s <- purrr::map_df(1:100, ~{
    mu <- runif(1, -100, 100)
    sd <- runif(1, 1, 5)
    min <- sample(-10:10, 1)
    max <- min + sample(1:10, 1)
    
    u <- runif(100, min, max)
    u2n <- unif2norm(u, mu, sd, min, max)
    u2 <- norm2unif(u2n, min, max, mu, sd)
    
    n <- rnorm(100, mu, sd)
    n2u <- norm2unif(n, min, max, mu, sd)
    n2 <- unif2norm(n2u, mu, sd, min, max)
    
    list(u = cor(u, u2), n = cor(n, n2))
  })
  
  expect_true(mean(s$u) > .99)
  expect_true(mean(s$n) > .99)
})

# unif2norm ----
test_that("unif2norm", {
  skip_on_cran()
  
  for (i in 1:reps) {
    # sample parameter values
    mu <- runif(1, -100, 100)
    sd <- runif(1, 1, 5)
    
    s <- purrr::map_df(mapreps, ~{
      x <- runif(n)  # sample orig dist
      y <- unif2norm(x, mu, sd) # convert
      y2 <- rnorm(n, mu, sd) # sample target dist
      
      # calculate relevant dist parameter
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2),
           # test converted & target dist are same
           p = suppressWarnings(ks.test(y, y2)$p.value))
    }) 
    
    # fails if converted & target dist differ too often
    expect_true(mean(s$p < .05) < .1)
    
    # get mean values for relevant parameters
    summ <- dplyr::summarise_all(s[1:4], mean) %>% unlist() %>% unname()
    # compare to sampled parameter values
    expect_equal(summ, c(mu, sd, mu, sd), tolerance = tol)
  }
})

# norm2unif ----
test_that("norm2unif", {
  skip_on_cran()
  
  for (i in 1:reps) {
    min <- runif(1, -100, 100)
    max <- min + runif(1, 1, 100)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(n)
      y <- norm2unif(x, min, max)
      y2 <- runif(n, min, max)
      list(min_1 = min(y), max_1 = max(y),
           min_2 = min(y2), max_2 = max(y2),
           p = suppressWarnings(ks.test(y, y2)$p.value))
    })
    
    expect_true(mean(s$p < .05) < .1)
    summ <- dplyr::summarise_all(s[1:4], mean) %>% unlist() %>% unname()
    expect_equal(summ, c(min, max, min, max), tolerance = tol)
  }
})

# norm2pois ----
test_that("norm2pois", {
  skip_on_cran()
  
  for (i in 1:reps) {
    lambda <- sample(1:10, 1)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(100)
      y <- norm2pois(x, lambda)
      
      y2 <- rpois(100, lambda)
      list(m_1 = mean(y), 
           m_2 = mean(y2),
           p = suppressWarnings(ks.test(y, y2)$p.value))
    })
    expect_true(mean(s$p < .05) < .1)
    
    summ <- dplyr::summarise_all(s[1:2], mean) %>% unlist() %>% unname()
    expect_equal(summ, c(lambda, lambda), tolerance = tol)
  }
})

# norm2gamma ----
test_that("norm2gamma", {
  skip_on_cran()
  
  for (i in 1:reps) {
    shape = sample(1:10, 1)
    rate = sample(1:10, 1)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(100)
      y <- norm2gamma(x, shape, rate)
      y2 <- rgamma(100, shape, rate)
      
      list(dist1_m = mean(y), 
           dist2_m = mean(y2),
           p = suppressWarnings(ks.test(y, y2)$p.value))
    })
    
    summ <- dplyr::summarise_all(s[1:2], mean) %>% unlist() %>% unname()
    expect_equal(summ, c(shape/rate, shape/rate), tolerance = tol)
    expect_true(mean(s$p < .05) < .1)
  }
})

# gamma2norm ----
test_that("gamma2norm", {
  skip_on_cran()
  
  for (i in 1:reps) {
    mu <- runif(1, -100, 100)
    sd <- runif(1, 1, 5)
    shape = sample(1:10, 1)
    rate = sample(1:10, 1)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rgamma(100, shape, rate)
      y <- gamma2norm(x, mu, sd, shape, rate)
      y2 <- rnorm(100, mu, sd)
      
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2),
           p = suppressWarnings(ks.test(y, y2)$p.value))
    }) 
    
    expect_true(mean(s$p < .05) < .1)
    summ <- dplyr::summarise_all(s[1:4], mean) %>% unlist() %>% unname()
    expect_equal(summ, c(mu, sd, mu, sd), tolerance = tol)
  }
})

# gamma ----
test_that("gamma", {
  # convert between normal and gamma and back to check 
  s <- purrr::map_df(1:100, ~{
    mu <- runif(1, -100, 100)
    sd <- runif(1, 1, 5)
    shape = sample(1:10, 1)
    rate = sample(1:10, 1)
    
    g <- rgamma(100, shape, rate)
    g2n <- gamma2norm(g, mu, sd, shape, rate)
    g2 <- norm2gamma(g2n, shape, rate, mu = mu, sd = sd)
    
    n <- rnorm(100, mu, sd)
    n2g <- norm2gamma(n, shape, rate, mu = mu, sd = sd)
    n2 <- gamma2norm(n2g, mu, sd, shape, rate)
    
    list(g = cor(g, g2), n = cor(n, n2))
  })
  
  expect_true(mean(s$g) > .99)
  expect_true(mean(s$n) > .99)
})

# beta ----
test_that("beta", {
  # convert between normal and beta and back to check 
  s <- purrr::map_df(1:100, ~{
    mu <- runif(1, -100, 100)
    sd <- runif(1, 1, 5)
    shape1 <- sample(1:10, 1)
    shape2 <- sample(1:10, 1)
    
    b <- rbeta(100, shape1, shape2)
    b2n <- beta2norm(b, mu, sd, shape1, shape2)
    b2 <- norm2beta(b2n, shape1, shape2, mu = mu, sd = sd)
    
    n <- rnorm(100, mu, sd)
    n2b <- norm2beta(n, shape1, shape2, mu = mu, sd = sd)
    n2 <- beta2norm(n2b, mu, sd, shape1, shape2)
    
    list(b = cor(b, b2), n = cor(n, n2))
  })
  
  expect_true(mean(s$b) > .99)
  expect_true(mean(s$n) > .99)
})

# norm2beta ----
test_that("norm2beta", {
  skip_on_cran()
  
  find_shape_params <- function(x) {
    mu <- mean(x)
    variance <- var(x)
    shape1 <- ((1 - mu) / variance - 1 / mu) * mu^2
    shape2 <- shape1 * (1 / mu - 1)
    return(list(shape1 = shape1, shape2 = shape2))
  }
  
  for (i in 1:reps) {
    shape1 <- sample(1:10, 1)
    shape2 <- sample(1:10, 1)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(100)
      y <- norm2beta(x, shape1, shape2)
      
      y2 <- rbeta(100, shape1, shape2)
      list(dist1_shape1 = find_shape_params(y)$shape1, 
           dist1_shape2 = find_shape_params(y)$shape2,
           dist2_shape1 = find_shape_params(y2)$shape1, 
           dist2_shape2 = find_shape_params(y2)$shape2,
           p = suppressWarnings(ks.test(y, y2)$p.value))
    })
    
    expect_true(mean(s$p < .05) < .1)
    summ <- dplyr::summarise_all(s[1:4], mean) %>% unlist() %>% unname()
    expect_equal(summ, c(shape1, shape2, shape1, shape2), tolerance = tol)
  }
})

# binom ----
test_that("binom", {
  # convert between normal and binom and back to check 
  s <- purrr::map_df(1:100, ~{
    mu <- runif(1, -100, 100)
    sd <- runif(1, 1, 5)
    size <- sample(5:20, 1)
    prob <- runif(1, .2, .8)
    
    b <- rbinom(100, size, prob)
    b2n <- binom2norm(b, mu, sd, size, prob)
    b2 <- norm2binom(b2n, size, prob, mu, sd)
    
    n <- rnorm(100, mu, sd)
    n2b <- norm2binom(n, size, prob, mu, sd)
    n2 <- binom2norm(n2b, mu, sd, size, prob)
    
    list(b = cor(b, b2), n = cor(n, n2))
  })
  
  expect_true(mean(s$b) > .99)
  expect_true(mean(s$n) > .95)
})

# norm2binom ----
test_that("norm2binom", {
  skip_on_cran()
  
  for (i in 1:reps) {
    prob <- runif(1)
    size <- sample(1:10, 1)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(100)
      y <- norm2binom(x, size, prob)
      
      y2 <- rbinom(100, size, prob)
      list(m_1 = mean(y), 
           m_2 = mean(y2),
           p = suppressWarnings(ks.test(y, y2)$p.value))
    })
    
    expect_true(mean(s$p < .05) < .1)
    summ <- dplyr::summarise_all(s[1:2], mean) %>% unlist() %>% unname()
    expect_equal(summ, c(prob*size, prob*size), tolerance = tol)
  }
})

# norm2nbinom ----
test_that("norm2nbinom", {
  skip_on_cran()
  
  for (i in 1:reps) {
    prob <- runif(1)
    size <- sample(1:10, 1)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(1000)
      y <- norm2nbinom(x, size, prob)
      
      y2 <- rnbinom(1000, size, prob)
      list(m_1 = mean(y), 
           m_2 = mean(y2),
           p = suppressWarnings(ks.test(y, y2)$p.value))
    })
    
    expect_true(mean(s$p < .05) < .1)
  }
})

test_that("nbinom", {
  # convert between nbinom and normal and back to check 
  set.seed(8675309)
  
  nbinom_check = function(size = 100, prob = .5, mu = 0, sd = 1) {
    x = rnbinom(1000, size, prob)
    y = nbinom2norm(x, mu, sd, size = size, prob = prob)
    z = norm2nbinom(y, size, prob, x_mu = mu, x_sd = sd)
    mean(x == z)
  }
  
  checks <- expand.grid(
    size = seq(5, 20, 5),
    prob = seq(.1, .9, .1),
    mu = -1:1,
    sd = 1:2
  )
  pcnt_identical = mapply(nbinom_check, checks$size, checks$prob, checks$mu, checks$sd)
  
  expect_true(all(pcnt_identical > .98))
})

# trunc ----
test_that("trunc", {
  # convert between normal and truncnorm and back to check 
  s <- purrr::map_df(1:100, ~{
    min <- runif(1, -100, 100)
    max <- min + runif(1, 1, 100)
    mu <- (max + min)/2
    sd <- (max - min)/runif(1, 2, 5)
    
    t <- truncnorm::rtruncnorm(100, min, max, mu, sd)
    t2n <- trunc2norm(t, min, max, mu, sd)
    t2 <- norm2trunc(t2n, min, max, mu, sd)
    
    n <- rnorm(100, mu, sd)
    n2t <- norm2trunc(n, min, max, mu, sd)
    n2 <- trunc2norm(n2t, min, max, mu, sd)
    
    list(t = cor(t, t2), n = cor(n, n2))
  })
  
  expect_true(mean(s$t) > .99)
  expect_true(mean(s$n) > .99)
})

# norm2trunc ----
test_that("norm2trunc", {
  skip_on_cran()
  
  for (i in 1:reps) {
    min <- runif(1, -100, 100)
    max <- min + runif(1, 1, 100)
    mu <- (max + min)/2
    sd <- (max - min)/runif(1, 2, 5)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(100, mu, sd)
      y <- norm2trunc(x, min, max, mu, sd)
      y2 <- truncnorm::rtruncnorm(100, min, max, mu, sd)
      
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2),
           p = suppressWarnings(ks.test(y, y2)$p.value))
    }) 
    
    expect_true(mean(s$p < .05) < .1)
    summ <- dplyr::summarise_all(s[1:4], mean) %>% unlist() %>% unname()
    # trunc m/sds not always possible to equal norm m/sd
    expect_equal(summ[1], summ[3], tolerance = tol)
    expect_equal(summ[2], summ[4], tolerance = tol)
  }
})


# trunc2norm ----
test_that("trunc2norm", {
  skip_on_cran()
  
  set.seed(3)
  x <- truncnorm::rtruncnorm(1000)
  
  expect_message(suppressWarnings(trunc2norm(x)), 
                 "-2\\.987\\d+ \\(min\\(x\\) = -3\\.056\\d+\\)")
  expect_message(suppressWarnings(trunc2norm(x)), 
                 "max was not set, so guessed as 3\\.000\\d+ \\(max\\(x\\) = 3\\.519\\d+\\)")
  expect_warning(suppressMessages(trunc2norm(x)), 
                 "min was > min\\(x\\), so min was set to -3\\.066\\d+")
  expect_warning(suppressMessages(trunc2norm(x)), 
                 "max was < max\\(x\\), so max was set to 3\\.529\\d+")
  
  set.seed(8675309)
  x <- truncnorm::rtruncnorm(100, mean = 10, sd = 5)
  
  expect_message(trunc2norm(x), "min was not set, so guessed as -3\\.675\\d+ \\(min\\(x\\) = -2\\.984\\d+\\)")
  expect_message(trunc2norm(x), "max was not set, so guessed as 24\\.198\\d+ \\(max\\(x\\) = 20\\.146\\d+\\)")
  
  # defaults
  for (i in 1:reps) {
    s <- purrr::map_df(mapreps, ~{
      x <- truncnorm::rtruncnorm(100)
      suppressMessages(suppressWarnings(y <- trunc2norm(x)))
      y2 <- rnorm(100)
      
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2),
           p = suppressWarnings(ks.test(y, y2)$p.value))
    }) 
    
    expect_true(mean(s$p < .05) < .1)
    
    summ <- dplyr::summarise_all(s[1:4], mean) %>% unlist() %>% unname()
    expect_equal(summ, c(0, 1, 0, 1), tolerance = tol)
  }
  
  # set min, max, mean and sd
  for (i in 1:reps) {
    min <- runif(1, -100, 100)
    max <- min + runif(1, 1, 100)
    mu <- (max + min)/2
    sd <- runif(1, 1, 5)
    
    s <- purrr::map_df(mapreps, ~{
      x <- truncnorm::rtruncnorm(100, min, max, mu, sd)
      suppressWarnings(
        y <- trunc2norm(x, min, max, mu, sd)
      )
      
      y2 <- rnorm(100, mu, sd)
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2),
           p = suppressWarnings(ks.test(y, y2)$p.value))
    }) 
    
    expect_true(mean(s$p < .05) < .1)
    summ <- dplyr::summarise_all(s[1:4], mean) %>% unlist() %>% unname()
    expect_equal(summ, c(mu, sd, mu, sd), tolerance = tol)
  }
})

# norm2likert ----
# test_that("norm2likert", {
#   skip_on_cran()
#   x <- rnorm(1e4)
#   
#   expect_error(norm2likert(), "argument \"prob\" is missing, with no default")
#   expect_error(norm2likert(x), "argument \"prob\" is missing, with no default")
#   
#   y <- norm2likert(x, c(.25, .5, .25))
#   expect_equal(mean(y == 1), .25, tolerance = tol)
#   expect_equal(mean(y == 2), .50, tolerance = tol)
#   expect_equal(mean(y == 3), .25, tolerance = tol)
#   
#   y <- norm2likert(x, c(.1, .2, .3, .4))
#   expect_equal(mean(y == 1), .1, tolerance = tol)
#   expect_equal(mean(y == 2), .2, tolerance = tol)
#   expect_equal(mean(y == 3), .3, tolerance = tol)
#   expect_equal(mean(y == 4), .4, tolerance = tol)
#   
#   y <- norm2likert(x, c(.4, .3, .2, .1))
#   expect_equal(mean(y == 1), .4, tolerance = tol)
#   expect_equal(mean(y == 2), .3, tolerance = tol)
#   expect_equal(mean(y == 3), .2, tolerance = tol)
#   expect_equal(mean(y == 4), .1, tolerance = tol)
#   
#   ## prob as counts
#   y <- norm2likert(x, c(40, 30, 20, 10))
#   expect_equal(mean(y == 1), .4, tolerance = tol)
#   expect_equal(mean(y == 2), .3, tolerance = tol)
#   expect_equal(mean(y == 3), .2, tolerance = tol)
#   expect_equal(mean(y == 4), .1, tolerance = tol)
#   
#   ## named prob
#   y <- norm2likert(x, c(a = 40, b = 30, c = 20, d = 10))
#   expect_equal(mean(y == "a"), .4, tolerance = tol)
#   expect_equal(mean(y == "b"), .3, tolerance = tol)
#   expect_equal(mean(y == "c"), .2, tolerance = tol)
#   expect_equal(mean(y == "d"), .1, tolerance = tol)
# })

# std_alpha2average_r ----
test_that("std_alpha2average_r", {
  skip("Too variable")
  set.seed(10)
  
  replicate(10, {
    n <- sample(10:100, 1)
    vars <- sample(5:20, 1)
    r <- runif(1)
    dat <- rnorm_multi(n, vars, r = r)
    suppressWarnings(capture.output(
      a <- psych::alpha(dat, check.keys = FALSE) %>% summary()
    ))
    calc_r <- std_alpha2average_r(a$std.alpha, vars)
    expect_equal(a$average_r, calc_r, tolerance = .001)
  })
})


# likert distributions ----
test_that("likert", {
  prob <- c(A= 1, B = 5, C = 10, D = 15, E = 20)
  
  # rlikert
  x <- rlikert(1e5, prob)
  #plot(x)
  # proportions are close
  count <- dplyr::count(data.frame(x = x), x)
  diffs <- abs(prob/sum(prob) - count$n/1e5)
  expect_true(all(diffs < .01))
  
  # dlikert
  x <- names(prob) %>% factor()
  d <- dlikert(x, prob)
  expect_equal(prob/sum(prob), d)
  
  # plikert
  q <- names(prob)
  p <- plikert(q, prob)
  #plot(q, p)
  diff <- cumsum(prob/sum(prob)) - p
  expect_true(all(abs(diff) < .0001))
  
  # plikert different order
  q2 <- rev(q)
  p2 <- plikert(q2, prob)
  diff <- rev(cumsum(prob/sum(prob))) - p2
  expect_true(all(abs(diff) < .0001))

  # qlikert
  q3 <- qlikert(p, prob)
  #plot(q3, p)
  expect_equal(as.factor(q), q3)
  
  p4 <- seq(0, 1, .01)
  q4 <- qlikert(p4, prob)
  #plot(as.numeric(q4), p4)
  
  counts <- data.frame(q = q4) %>% dplyr::count(q)
  expect_equal(cumsum(counts$n), floor(unname(p)*100) + 1)
  
})

test_that("likert labels", {
  prob <- c(.1, .2, .4, .2, .1)
  labels <- -2:2
  
  # rlikert
  x <- rlikert(1e5, prob, labels)
  #plot(as.factor(x))
  # proportions are close
  count <- dplyr::count(data.frame(x = x), x)
  diffs <- abs(prob/sum(prob) - count$n/1e5)
  expect_true(all(diffs < .01))
  
  # dlikert
  x <- labels
  d <- dlikert(x, prob, labels)
  expect_equal(prob/sum(prob), unname(d))
  
  # plikert
  q <- labels
  p <- plikert(q, prob, labels)
  #plot(q, p)
  diff <- cumsum(prob/sum(prob)) - p
  expect_true(all(abs(diff) < .0001))
  
  # plikert different order
  q2 <- rev(q)
  p2 <- plikert(q2, prob, labels)
  diff <- rev(cumsum(prob/sum(prob))) - p2
  expect_true(all(abs(diff) < .0001))
  
  # qlikert
  # q3 <- qlikert(p, prob, labels)
  # #plot(q3, p)
  # expect_equal(q, q3)
  # 
  # p4 <- seq(0, 1, .01)
  # q4 <- qlikert(p4, prob, labels)
  # #plot(as.numeric(q4), p4)
  # 
  # counts <- data.frame(q = q4) %>% dplyr::count(q)
  # expect_equal(cumsum(counts$n), floor(unname(p)*100) + 2)
})

# scale distributions ----
test_that("rscale", {
  replicate(200, {
    a <- sample(0:10, 1)
    b <- a + sample(1:10, 1)
    mean <- sample(seq(a, b, .1), 1)
    sd <- sample(seq(.1, b-a, .1), 1)
    
    x <- rscale(1000, a, b, mean, sd)
    expect_true(all(x %in% a:b))
  })
})
