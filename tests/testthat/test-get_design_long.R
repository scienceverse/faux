user_opts <- faux_options("sep", "verbose", "plot", "connection")
on.exit(faux_options(user_opts))

faux_options(plot = FALSE)

# 2w ----
test_that("2w", {
  within <- list(time = c("day", "night"))
  between <- list()
  mu <- c(1,2)
  d <- check_design(within, between, mu = mu)
  data <- sim_data(d, long = TRUE, empirical= TRUE)
  d2 <- get_design_long(data)
  
  expect_equal(d$within, d2$within)
  expect_equivalent(d$between, d2$between)
  expect_equal(d$n, d2$n)
  expect_equal(d$mu, d2$mu)
  expect_equal(d$sd, d2$sd)
  expect_equal(d$r, d2$r)
})

# get_design_long ----
test_that("get_design_long", {
  design <- check_design(2, 2, n = c(10, 20), mu = 1:4, sd = 4:1, r = 0.5)
  df_long <- sim_design(design = design, long = TRUE, empirical = TRUE)
  d <- get_design_long(df_long)
  
  n <- list(B1a = 10, B1b = 20)
  mu <- list(B1a = list(W1a = 1, W1b = 2), B1b = list(W1a = 3, W1b = 4))
  sd <- list(B1a = list(W1a = 4, W1b = 3), B1b = list(W1a = 2, W1b = 1))
  r <- data.frame(W1a = c(1, .5), W1b = c(.5, 1), row.names = c("W1a", "W1b")) %>% as.matrix()
  
  expect_equal(d$within, list(W1 = list(W1a="W1a", W1b="W1b")))
  expect_equal(d$between, list(B1 = list(B1a="B1a", B1b="B1b")))
  expect_equal(d$n, n)
  expect_equal(d$mu, mu)
  expect_equal(d$sd, sd)
  expect_equal(d$r$B1a, r)
  expect_equal(d$r$B1b, r)
  
  data <- sim_design(c(2, 2, 2), c(2, 2, 2), long = TRUE, 
                        empirical = TRUE)
  d <- get_design_long(data)
  
  expect_equal(d$mu[[1]] %>% names(), 
               c("W1a_W2a_W3a", "W1a_W2a_W3b", "W1a_W2b_W3a", "W1a_W2b_W3b", 
                 "W1b_W2a_W3a", "W1b_W2a_W3b", "W1b_W2b_W3a", "W1b_W2b_W3b"))
  expect_equal(d$mu %>% names(), 
               c("B1a_B2a_B3a", "B1a_B2a_B3b", "B1a_B2b_B3a", "B1a_B2b_B3b",
                 "B1b_B2a_B3a", "B1b_B2a_B3b", "B1b_B2b_B3a", "B1b_B2b_B3b"))
  expect_equal(d$n %>% unlist() %>% unname(), rep(100, 8))
  expect_equal(d$mu %>% unlist() %>% unname(), rep(0, 64))
  expect_equal(d$sd %>% unlist() %>% unname(), rep(1, 64))
  expect_equal(d$r[[1]] %>% sum(), 8)
})

# 2w*2b ----
test_that("2w*2b", {
  within <- list(time = c("night", "day"))
  between <- list(pet = c("dog", "cat"))
  mu <- list(
    dog = c(1,2),
    cat = c(2,3)
  )
  d <- check_design(within, between, mu = mu)
  data <- sim_design(within, between, mu = mu, long = TRUE, 
                     empirical = TRUE)
  d2 <- get_design_long(data)
  expect_equal(d$within, d2$within)
  expect_equivalent(d$between, d2$between)
  expect_equal(d$n, d2$n)
  expect_equal(d$mu, d2$mu)
  expect_equal(d$sd, d2$sd)
  expect_equal(d$r, d2$r)
})

# complex ----
test_that("complex", {
  within <- c(2, 3)
  between <- c(2, 3)
  id <- c(sub_id = "ID")
  dv <- c(dv = "My DV")
  d <- check_design(within, between, dv = dv, id = id)
  data <- sim_design(within, between, dv = dv, id = id, 
                     empirical = TRUE, long = TRUE)
  d2 <- get_design_long(data, dv = dv, id = id)
  expect_equal(d, d2)
})
