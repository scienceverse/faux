user_opts <- faux_options("sep", "verbose", "plot", "connection")
on.exit(faux_options(user_opts))

faux_options(plot = FALSE)

# error messages ----
test_that("error messages", {
  list_err <- "within and between must be lists"
  expect_error(sim_design("1"), list_err)
  expect_error(sim_design(list(), "1"), list_err)
  
  factor_name_err <- "You have multiple factors with the same name \\(A\\). Please give all factors unique names."
  within <- list("A" = c("A1", "A2"))
  between <- list("A" = c("A1", "A2"))
  expect_error(sim_design(within, between), factor_name_err)
  
  level_err <- "You have duplicate levels for factor\\(s\\): A, C, B, D"
  within <- list("A" = c("yes", "yes"), "C" = c("C1", "C1"))
  between <- list("B" = c("B1", "B1"), "D" = c("D1", "D1"))
  expect_error(sim_design(within, between), level_err)
  
  level_err <- "You have duplicate levels for factor\\(s\\): A, B"
  within <- list("A" = c("yes", "yes"), "C" = c("C1", "C2"))
  between <- list("B" = c("yes", "yes"), "D" = c("D1", "D2"))
  expect_error(sim_design(within, between), level_err)
  
  expect_error(sim_design(rep = "A"), "rep must be a number")
  expect_error(sim_design(rep = -2), "rep must be >= 1")
  expect_warning(sim_design(rep = 2.2), "rep should be an integer")
})

# set mu ----
test_that("mu", {
  w <- list("A" = c("A1", "A2"))
  x <- sim_design(within = w, mu = 1, empirical = TRUE)
  expect_equal(mean(x$A1), 1, tolerance = 1e3)
  expect_equal(mean(x$A2), 1, tolerance = 1e3)
  
  x <- sim_design(within = w, mu = c(1, 2), empirical = TRUE)
  expect_equal(mean(x$A1), 1, tolerance = 1e3)
  expect_equal(mean(x$A2), 2, tolerance = 1e3)
  
  x <- sim_design(within = w, mu = c(A2 = 2, A1 = 1), empirical = TRUE)
  expect_equal(mean(x$A1), 1, tolerance = 1e3)
  expect_equal(mean(x$A2), 2, tolerance = 1e3)
  
  x <- sim_design(within = w, mu = list(A2 = 2, A1 = 1), empirical = TRUE)
  expect_equal(mean(x$A1), 1, tolerance = 1e3)
  expect_equal(mean(x$A2), 2, tolerance = 1e3)
  
  x <- sim_design(within = w, mu = data.frame(A2 = 2, A1 = 1), empirical = TRUE)
  expect_equal(mean(x$A1), 1, tolerance = 1e3)
  expect_equal(mean(x$A2), 2, tolerance = 1e3)
  
  x <- sim_design(within = w, mu = data.frame(y = 2:1, row.names = c("A2", "A1")), empirical = TRUE)
  expect_equal(mean(x$A1), 1, tolerance = 1e3)
  expect_equal(mean(x$A2), 2, tolerance = 1e3)
})

# 2w ----
test_that("2w", {
  within <- list(
    "W" = c("W1", "W2")
  )
  between <- list()
  mu <- c(1, 2)
  sd <- c(1, 2)
  r <- 0.3
  dv <- list("rt" = "Reaction Time")
  id <- list("sub_id" = "Subject ID")
  n <- 100
  
  df <- sim_design(within, between, mu = mu, sd = sd, 
                   r = r, dv = dv, id = id, empirical = TRUE)
  chk <- check_sim_stats(df)
  
  comp <- data.frame(
    n = c(100, 100),
    var = factor(c("W1", "W2")),
    W1 = c(1.0, 0.3),
    W2 = c(0.3, 1.0),
    mean = c(1, 2),
    sd = c(1, 2)
  )
  
  attr <- attributes(df)
  expect_true("design" %in% names(attr))
  expect_equal(attr$design$within, list(W = list(W1 = "W1", W2 = "W2")))
  expect_equal(attr$design$between, list())
  expect_equal(attr$design$dv, dv)
  expect_equal(attr$design$id, id)
  expect_equal(attr$design$n %>% unlist() %>% sum(), 100)
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 3)
  expect_equal(names(df), c("sub_id", "W1", "W2"))
  expect_equivalent(chk, comp)
})

# 2w*2w ----
test_that("2w*2w", {
  within <- list(
    "W" = c("W1", "W2"),
    "X" = c("X1", "X2")
  )
  between <- list()
  
  df <- sim_design(within, between, empirical = TRUE)
  chk <- check_sim_stats(df)

  comp <- data.frame(
    n = rep(100, 4),
    var = factor(c("W1_X1", "W1_X2", "W2_X1", "W2_X2")),
    W1_X1 = c(1, 0, 0, 0),
    W1_X2 = c(0, 1, 0, 0),
    W2_X1 = c(0, 0, 1, 0),
    W2_X2 = c(0, 0, 0, 1),
    mean = rep(0, 4),
    sd = rep(1, 4)
  )
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 5)
  expect_equal(names(df), c("id", "W1_X1", "W1_X2", "W2_X1", "W2_X2"))
  expect_equivalent(chk, comp)
})

# 2b ----
test_that("2b", {
  between <- list(
    "B" = c("B1", "B2")
  )
  within <- list()
  mu <- c(1, 2)
  
  df <- sim_design(within, between, n = 100, mu = mu, 
                   empirical = TRUE)
  chk <- get_params(df, between = "B")
  
  comp <- data.frame(
    B = factor(c("B1","B2")),
    n = c(100, 100),
    mean = c(1, 2),
    sd = c(1, 1)
  )
  
  expect_equal(nrow(df), 200)
  expect_equal(ncol(df), 3)
  expect_equal(names(df), c("id", "B", "y"))
  expect_equivalent(chk, comp)
})

# 2b*2b ----
test_that("2b*2b", {
  between <- list(
    "A" = c("A1", "A2"),
    "B" = c("B1", "B2")
  )
  within <- list()
  
  df <- sim_design(within, between, n = 100, mu = 1:4,
                   empirical = TRUE)
  chk <- check_sim_stats(df, between = c("A","B"))
  
  comp <- data.frame(
    A = factor(c("A1", "A1", "A2", "A2"), 
               levels = c("A1", "A2")),
    B = factor(c("B1", "B2", "B1", "B2"), 
               levels = c("B1", "B2")),
    n = rep(100, 4),
    mean = 1:4,
    sd = rep(1, 4) 
  )
  
  expect_equal(nrow(df), 400)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("id", "A", "B", "y"))
  expect_equivalent(chk, comp)
})

# 2w*2b basic ----
# uses ordering for within specification (not labels)
test_that("2w*2b basic", {
  between <- list(
    "B" = c("B1", "B2")
  )
  within <- list(
    "W" = c("W1", "W2")
  )
  n <- list(
    "B1" = 60,
    "B2" = 40
  )
  mu <- list(
    "B1" = c(10, 20),
    "B2" = c(10, 30)
  )
  sd <- list(
    "B1" = c(3, 4),
    "B2" = c(5, 6)
  )
  r <- list(
    "B1" = .2,
    "B2" = .5
  )
  
  df <- sim_design(within, between, n, mu, sd, r, TRUE)
  chk <- check_sim_stats(df, between = "B")
  
  comp <- data.frame(
    B = factor(c("B1", "B1", "B2", "B2"), c("B1", "B2")),
    n = c(60, 60, 40, 40),
    var = factor(c("W1", "W2", "W1", "W2")),
    W1 = c(1, .2, 1, .5),
    W2 = c(.2, 1, .5, 1),
    mean = c(10, 20, 10, 30),
    sd = 3:6
  )
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("id", "B", "W1", "W2"))
  expect_equivalent(chk, comp)
})

# 2w*2b alt ----
# uses alternative specification for factors
test_that("2w*2b alt", {
  between <- list(
    "B" = c(B1 = "First between level", B2 = "Second between level")
  )
  within <- list(
    "W" = c(W1 = "First within level", W2 = "Second within level")
  )
  n <- list(
    B1 = 60,
    B2 = 40
  )
  mu <- list(
    B1 = c(10, 20),
    B2 = c(10, 30)
  )
  sd <- list(
    B1 = c(3, 4),
    B2 = c(5, 6)
  )
  r <- list(
    B1 = .2,
    B2 = .5
  )
  
  df <- sim_design(within, between, n, mu, sd, r, TRUE)
  chk <- check_sim_stats(df, between = "B")
  
  comp <- data.frame(
    B = factor(c("B1", "B1", "B2", "B2"), c("B1", "B2")),
    n = c(60, 60, 40, 40),
    var = factor(c("W1", "W2", "W1", "W2")),
    W1 = c(1, .2, 1, .5),
    W2 = c(.2, 1, .5, 1),
    mean = c(10, 20, 10, 30),
    sd = 3:6
  )
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("id", "B", "W1", "W2"))
  expect_equivalent(chk, comp)
})


# 2w*2b within order ----
test_that("2w*2b within order", {
  between <- list(
    "B" = c("B1", "B2")
  )
  within <- list(
    "W" = c("W1", "W2")
  )
  
  mu <- list(
    B2 = c(W2 = 30, W1 = 10),
    B1 = c(W2 = 20, W1 = 10)
  )
  
  sd <- list(
    "B1" = c(W2 = 4, W1 = 3),
    "B2" = c(W2 = 6, W1 = 5)
  )
  
  df <- sim_design(within, between, 50, mu, sd, .5, TRUE)
  chk <- check_sim_stats(df, between = "B")
  comp <- data.frame(
    B = factor(c("B1", "B1", "B2", "B2"), c("B1", "B2")),
    n = rep(50, 4),
    var = factor(c("W1", "W2", "W1", "W2")),
    W1 = c(1, .5, 1, .5),
    W2 = c(.5, 1, .5, 1),
    mean = c(10, 20, 10, 30),
    sd = 3:6
  )
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("id", "B", "W1", "W2"))
  expect_equivalent(chk, comp)
})


# 2w*2b order ----
# change order of named list items
test_that("2w*2b order", {
  between <- list(
    "B" = c("B1", "B2")
  )
  within <- list(
    "W" = c("W1", "W2")
  )
  # if you try to specify n for each level of W, it will just use the first level
  # TODO: add a warning for this
  n <- list(
    "B2" = 40,
    "B1" = 60
  )
  mu <- list(
    "B2" = c(W1 = 10, W2 = 30),
    "B1" = c(W1 = 10, W2 = 20)
  )
  sd <- list(
    "B2" = c(W1 = 5, W2 = 6),
    "B1" = c(W1 = 3, W2 = 4)
  )
  r <- list(
    "B2" = .5,
    "B1" = .2
  )
  
  df <- sim_design(within, between, n, mu, sd, r, TRUE)
  chk <- check_sim_stats(df, between = "B")
  comp <- data.frame(
    B = factor(c("B1", "B1", "B2", "B2"), c("B1", "B2")),
    n = c(60, 60, 40, 40),
    var = factor(c("W1", "W2", "W1", "W2")),
    W1 = c(1, .2, 1, .5),
    W2 = c(.2, 1, .5, 1),
    mean = c(10, 20, 10, 30),
    sd = 3:6
  )
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("id", "B", "W1", "W2"))
  expect_equivalent(chk, comp)
})

# 2w*2b*2b ----
test_that("2w*2b*2b", {
  between <- list(
    A = c("A1", "A2"),
    B = c("B1", "B2")
  )
  within <- list(
    W = c("W1", "W2")
  )
  n <- list(
    A1_B1 = 50,
    A2_B1 = 50,
    A1_B2 = 50,
    A2_B2 = 50
  )
  mu <- list(
    A1_B1 = c(W1 = 10, W2 = 20),
    A2_B1 = c(W1 = 30, W2 = 40),
    A1_B2 = c(W1 = 50, W2 = 60),
    A2_B2 = c(W1 = 70, W2 = 80)
  )
  sd <- list(
    A1_B1 = c(W1 = 3, W2 = 4),
    A2_B1 = c(W1 = 5, W2 = 6),
    A1_B2 = c(W1 = 7, W2 = 8),
    A2_B2 = c(W1 = 9, W2 = 10)
  )
  r <- list(
    A1_B1 = .1,
    A2_B1 = .2,
    A1_B2 = .3,
    A2_B2 = .4
  )
  
  df <- sim_design(within, between, n, mu, sd, r, TRUE)
  check_sim_stats(df, between = c("A", "B"))
  
  expect_equal(nrow(df), 200)
  expect_equal(ncol(df), 5)
  expect_equal(names(df), c("id", "A", "B", "W1", "W2"))
})

# long format ----
test_that("long", {
  between <- list(
    "B" = c("B1", "B2"),
    "A" = c("A2", "A1")
  )
  within <- list(
    "W" = c("W1", "W2"),
    "C" = c("C2", "C1"),
    "N" = c("N2", "N1")
  )
  
  df <- sim_design(within, between, 100, 0, 1, .5, 
                   empirical = TRUE, long = TRUE)
  
  expect_equal(nrow(df), 3200)
  expect_equal(ncol(df), 7)
  expect_equal(names(df), c("id", "B", "A", "W", "C", "N", "y"))
})

# names with the sep ----
test_that("complex names", {
  within <- list(A = c("A_1", "A_2"), Z = c("Z_1", "Z_2"))

  expect_error(sim_design(within, long = TRUE))
  expect_error(sim_design(within))
  
  expect_silent(sim_design(within, long = TRUE, sep = "."))
  expect_silent(sim_design(within, sep = "."))

})

# same factor level names ----
test_that("same factor level names", {
  between <- list(
    pets = c("cats", "dogs"),
    pets2 = c("cats", "dogs")
  )
  within <- list(
    time = c("day", "night"),
    time2 = c("day", "night")
  )
  
  df_long <- sim_design(within, between, 10, 0, 1, .5, TRUE, TRUE)
  df_wide <- sim_design(within, between, 10, 0, 1, .5, TRUE, FALSE)
                        
  long_names <- c("id", "pets", "pets2", "time", "time2",  "y")
  wide_names <- c("id", "pets", "pets2", "day_day", "day_night", "night_day", "night_night")
  
  expect_equal(names(df_long), long_names)
  expect_equal(names(df_wide), wide_names)
})

# other stuff ----
test_that("works", {
  between <- list(
    "B" = c("B1", "B2"),
    "A" = c("A2", "A1")
  )
  within <- list(
    "W" = c("W1", "W2"),
    "C" = c("C2", "C1")
  )
  
  mu = list(
    "B1_A2" = c(0, 10, 20, 30),
    "B1_A1" = c(40, 50, 60, 70),
    "B2_A1" = c(100, 110, 120, 130),
    "B2_A2" = c(140, 150, 160, 170)
  )
  sd = list(
    "B1_A2" = c(1, 1, 1, 1),
    "B1_A1" = 2,
    "B2_A1" = c(5, 10, 15, 20),
    "B2_A2" = c(30, 40, 50, 60)
  )
  
  triangle <- c(.1, .2, .3, .4, .5, .6)
  long_cor <- c(1, .1, .2, .3,
               .1,  1, .4, .5,
               .2, .4,  1, .6,
               .3, .5, .6,  1)
  mat <- matrix(long_cor, nrow = 4)
  
  r = list(
    "B1_A2" = triangle,
    "B1_A1" = long_cor,
    "B2_A1" = mat,
    "B2_A2" = .4
  )
  
  n = 100
  empirical = TRUE

  df <- sim_design(within, between, n, mu, sd, r, empirical)
  check_sim_stats(df, c("B", "A"))
  
  expect_equal(nrow(df), 400)
  expect_equal(ncol(df), 7)
  expect_equal(names(df), c("id", "B", "A", "W1_C2", "W1_C1", "W2_C2", "W2_C1"))
})

# label order ----
test_that("label order", {
  within <- list(
    pets = c("ferret", "dog", "cat")
  )
  between <- list(
    time = c("night", "day")
  )
  df <- sim_design(within, between, long = TRUE)
  
  expect_true(is.factor(df$pets))
  expect_true(is.factor(df$time))
  expect_equal(levels(df$pets), c("ferret", "dog", "cat"))
  expect_equal(levels(df$time), c("night", "day"))
})

# seed ----
test_that("seed", {
  # # setting seed returns same DF, but is reset
  # set.seed(1)
  # rnd0 <- rnorm(1)
  # df1 <- sim_design(2, 2, n = 10, seed = 910210)
  # rnd1 <- rnorm(1)
  # df2 <- sim_design(2, 2, n = 10, seed = 910210)
  # rnd2 <- rnorm(1)
  # set.seed(1)
  # rnd0b <- rnorm(1)
  # rnd1b <- rnorm(1)
  # rnd2b <- rnorm(1)
  # df3 <- sim_design(2, 2, n = 10, seed = 8675309)
  # 
  # expect_equal(df1, df2)
  # 
  # expect_false(rnd1 == rnd2)
  # expect_equal(rnd0, rnd0b)
  # expect_equal(rnd1, rnd1b)
  # expect_equal(rnd2, rnd2b)
  # expect_true(!identical(df1, df3))
  
  # user sets seed externally
  set.seed(1)
  df4 <- sim_design(2, 2, n = 10)
  set.seed(1)
  df5 <- sim_design(2, 2, n = 10)
  expect_equal(df4, df5)
})

# from design ----
test_that("from design", {
  within <- list(time = c("night", "day"))
  between <- list(pet = c("dog", "cat"))
  design <- check_design(within, between, n = 10)
  data <- sim_design(design = design)
  
  expect_equal(attributes(data)$design, design)
  
  # design set to first (within) argument
  data2 <- sim_design(design)
  
  expect_equal(attributes(data2)$design, design)
  
})

# small empirical ----
# test_that("small empirical", {
#   data <- sim_design(2, n = 2, r = 0.5, empirical = TRUE)
#   
# })

# multiple reps ----
test_that("multiple reps", {
  rep <- 9
  n <- 10
  df <- sim_design(2, n = n, rep = rep, plot = FALSE)
  
  expect_equal(nrow(df), rep)
  expect_equal(nrow(df$data[[1]]), n)
  expect_false(isTRUE(all.equal(df$data[[1]], df$data[[2]], 
                                check.environment=FALSE)))
  expect_equal(names(df$data[[1]]), c("id", "W1a", "W1b"))
  expect_equal(nrow(df$data[[1]]), n)
  
  df <- sim_design(2, n = n, rep = rep, 
                   long = TRUE, plot = FALSE)
  
  expect_equal(nrow(df), rep)
  expect_equal(nrow(df$data[[1]]), 2*n)
  expect_false(isTRUE(all.equal(df$data[[1]], df$data[[2]], 
                      check.environment=FALSE)))
  expect_equal(names(df$data[[1]]), c("id", "W1", "y"))
  expect_equal(nrow(df$data[[1]]), n*2)
})

# unnested reps ----
test_that("unnested reps", {
  rep <- 5
  n <- 10
  df <- sim_design(2, n = n, rep = rep, nested = FALSE, plot = FALSE)
  expect_equal(nrow(df), rep*n)
  expect_equivalent(df$rep, rep(1:rep, each = n), )
})

# empirical ----
test_that("empirical", {
  tol = .000001
  A <- list(A = c("A1", "A2"))
  for (i in 1:10) {
    for (n in seq(10,30, 10)) {
      df <- sim_design(A, r = 0.5, n = n, empirical = TRUE, plot = FALSE)
      
      # equal to parameters within tolerance
      expect_equal(cor(df$A1, df$A2), 0.5, tolerance = tol)
      expect_equal(mean(df$A1), 0, tolerance = tol)
      expect_equal(mean(df$A2), 0, tolerance = tol)
      expect_equal(sd(df$A1), 1, tolerance = tol)
      expect_equal(sd(df$A1), 1, tolerance = tol)
    }
  }
  
  m1 = c(); m2 = c(); sd1 = c(); sd2 = c(); r = c();
  for (i in 1:100) {
    tol = .000001
    df <- sim_design(A, n = 10, r = 0.5, empirical = FALSE, plot = FALSE)
    r[i] <- abs(cor(df$A1, df$A2)-0.5)
    m1[i] <- abs(mean(df$A1))
    m2[i] <- abs(mean(df$A2))
    sd1[i] <- abs(sd(df$A1)-1)
    sd2[i] <- abs(sd(df$A2)-1)
  }
  
  # most at least .1 off empirical
  expect_true(mean(r>.1) > .5)
  expect_true(mean(m1>.1) > .5)
  expect_true(mean(m2>.1) > .5)
  expect_true(mean(sd1>.1) > .5)
  expect_true(mean(sd2>.1) > .5)
})

# interactive ----
test_that("interactive", {
  f <- file()
  faux_options(connection = f)
  c("0", "0", "A", "B", "10", "100", "10") %>%
    paste(collapse = "\n") %>%
    write(f)
  
  x <- capture_output_lines(d <- sim_design(interactive = TRUE))
  
  expect_equal(nrow(d), 10)
  expect_equal(names(d), c("B", "A"))
  
  close(f)
})

# sep ----
test_that("sep", {
  within = list(A = c("A_1", "A.2", "A-3"),
                B = c("B_1", "B.2", "B-3"))
  
  between = list(C = c("C_1", "C.2", "C-3"),
                 D = c("D_1", "D.2", "D-3"))
  
  alevels <- factor(c("A_1", "A.2", "A-3"), levels = c("A_1", "A.2", "A-3"))
  
  faux_options(sep = "~")
  datw <- sim_design(within, between, n=10)
  datl <- sim_design(within, between, n=10, long = TRUE)
  
  nm <- c("id", "C", "D", 
          "A_1~B_1", "A_1~B.2", "A_1~B-3", 
          "A.2~B_1", "A.2~B.2", "A.2~B-3", 
          "A-3~B_1", "A-3~B.2", "A-3~B-3")
  expect_equal(names(datw), nm)
  expect_equal(unique(datl$A), alevels) 

  
  # shirdekel example: ignore sep if <2 factors win or btwn
  between <- list(condition = c(
    control = "Control",
    low_choice = "Low choice", 
    high_choice = "High choice"
  ))
  within <- list(time = c("Pre-essay", "Post-essay"))
  
  faux_options(sep = "~")
  mu <- data.frame(
    control = c(2, 2),
    low_choice = c(2, 3),
    high_choice = c(2, 5),
    row.names = within$time
  )
  
  dat <- sim_design(within, between,
             n = 10, mu = mu, sd = 2, r = .5,
             empirical = TRUE, plot = FALSE
  )
  faux_options(sep = "_")
})

# vardesc ----
test_that("vardesc", {
  between <- list(
    B = c(B1 = "Level 1B", B2 = "Level 2B")
  )
  within <- list(
    W = c(W1 = "Level 1W", W2 = "Level 2W")
  )
  
  vardesc <- list(B = "Between-Subject Factor",
                  W = "Within-Subject Factor")
  
  expect_silent(dat <- sim_design(within, between, vardesc = vardesc))
  design <- get_design(dat)
  expect_mapequal(design$vardesc, vardesc)
})


# numeric levels ----
test_that("numeric levels", {
  f <- list(int = 2:3, 
            num = c(2.2, 3.3),
            char = LETTERS[2:3],
            bool = c(TRUE, FALSE))
  
  data <- sim_design(n = 1, between = f)
  expect_true(is.integer(data$int))
  expect_true(is.numeric(data$num))
  expect_true(is.factor(data$char))
  expect_true(is.logical(data$bool))
  
  data <- sim_design(n = 1, within = f, long = TRUE)
  expect_true(is.integer(data$int))
  expect_true(is.numeric(data$num))
  expect_true(is.factor(data$char))
  expect_true(is.logical(data$bool))
})
