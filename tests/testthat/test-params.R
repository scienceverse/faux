test_that("error messages", {
  expect_error(sample_params("A"), "data must be a data frame or matrix")
  expect_error(sample_params(iris, FALSE), "between must be a numeric or character vector")
})

test_that("defaults", {
  checkiris <- sample_params(iris)
  irisnames <- c("var", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "n", "mean", "sd")
  
  expect_equal(nrow(checkiris), 4)
  expect_equal(ncol(checkiris), 8)
  expect_equal(names(checkiris), irisnames)
})

test_that("defaults with between", {
  checkiris <- sample_params(iris, "Species")
  irisnames <- c("Species", "var", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "n", "mean", "sd")
  
  expect_equal(nrow(checkiris), 12)
  expect_equal(ncol(checkiris), 9)
  expect_equal(names(checkiris), irisnames)
})

test_that("long", {
  data <- sim_design(within = 2, between = 2, r = 0.5, 
                        empirical = TRUE, long = TRUE, plot = FALSE)
  checklong <- sample_params(data)
  
  expect_equal(checklong$B1, c("B1a", "B1a", "B1b", "B1b") %>% as.factor())
  expect_equal(checklong$n, c(100,100,100,100))
  expect_equal(checklong$W1, factor(c("W1a", "W1b", "W1a", "W1b")))
  expect_equal(checklong$mean, c(0,0,0,0))
  expect_equal(checklong$sd, c(1,1,1,1))
  expect_equal(checklong$W1a, c(1,.5,1,.5))
  expect_equal(checklong$W1b, c(.5,1,.5,1))
})

test_that("is_pos_def", {
  expect_equal(is_pos_def(matrix(c(1, .5, .5, 1), 2)), TRUE)
  
  bad_matrix <- matrix(c(1, .9, .9, 
                        .9, 1, -.2,
                        .9, -.2, 1), 3)
  expect_equal(is_pos_def(bad_matrix), FALSE)
})


test_that("order", {
  data <- sim_design(
    within = list(time = c("pre", "post"),
                  condition = c("ctl", "exp"))
  )
  
  p <- sample_params(data)
  expect_equal(p$time, rep(c("pre", "post"), each = 2))
  expect_equal(p$condition, rep(c("ctl", "exp"), times = 2))
  
  data <- sim_design(
    between = list(grp = c("B", "A")),
    within = list(time = c("pre", "post"),
                  condition = c("ctl", "exp"))
  )
  p <- sample_params(data, between = "grp")
  expect_equal(as.character(p$grp), 
               rep(LETTERS[2:1], each = 4))
  expect_equal(as.character(p$time), 
               rep(c("pre", "post"), times = 2, each = 2))
})


test_that("from design", {
  x <- sim_design(
    between = list(grp = c("B", "A")),
    within = list(time = c("pre", "post"),
                  condition = c("ctl", "exp"))
  )
  p <- sample_params(x)
  expect_equal(as.character(p$grp), rep(LETTERS[2:1], each = 4))
  expect_equal(as.character(p$time), 
               rep(c("pre", "post"), times = 2, each = 2))
  expect_equal(as.character(p$condition), 
               rep(c("ctl", "exp"), times = 4))
  
  # override between
  p <- sample_params(x, between = 0)
  expect_true(!"grp" %in% names(p))
  expect_equal(as.character(p$time), 
               rep(c("pre", "post"), each = 2))
  expect_equal(as.character(p$condition), 
               rep(c("ctl", "exp"), times = 2))
  
  # override dv
  p <- sample_params(x, dv = c("pre_exp", "post_ctl"))
  expect_equal(as.character(p$grp), rep(LETTERS[2:1], each = 2))
  expect_equal(as.character(p$time), rep(c("pre", "post"), 2))
  expect_equal(as.character(p$condition), rep(c("exp", "ctl"), 2))
  
  # when long
  x <- sim_design(
    between = list(grp = c("B", "A")),
    within = list(time = c("pre", "post"),
                  condition = c("ctl", "exp")),
    long = TRUE
  )
  p <- sample_params(x)
  expect_equal(as.character(p$grp), rep(LETTERS[2:1], each = 4))
  expect_equal(as.character(p$time), 
               rep(c("pre", "post"), times = 2, each = 2))
  expect_equal(as.character(p$condition), 
               rep(c("ctl", "exp"), times = 4))
})

test_that("aliases", {
  expect_equal(check_sim_stats, sample_params)
  expect_equal(sample_params, sample_params)
})





  