test_that("add_ranef", {
  data <- expand.grid(
    rater = 1:3,
    stimulus = 1:2,
    obs = 1:4
  )
  
  datar <- data %>%
    add_ranef("rater", r_i = 1) %>%
    add_ranef("stimulus", s_i = 10) %>%
    add_ranef(c("rater", "stimulus"), rs_i = 100)
  
  r <- datar$r_i
  s <- datar$s_i
  rs <- datar$rs_i
  
  expect_equal(rep(r[1:3], 8), r)
  expect_equal(rep(s[c(1, 4)], each = 3, times = 4), s)
  expect_equal(rep(rs[1:6], 4), rs)
})

test_that("add_ranef 2", {
  set.seed(1)
  
  nrater <- 5000
  x_sd <- sample(1:10, 1)
  y_sd <- sample(1:10, 1)
  r_xy <- 0.5
  
  data <- expand.grid(
    rater = 1:nrater,
    stimulus = 1:2
  )
  
  datar <- add_ranef(data, "rater", x = x_sd, y = y_sd, .cors = r_xy)
  x <- datar$x[1:nrater]
  y <- datar$y[1:nrater]
  
  expect_equal(x, datar$x[(nrater+1):(2*nrater)])
  
  expect_true(mean(x) %>% abs() < .1)
  expect_true(mean(y) %>% abs() < .1)
  expect_equal(sd(x), x_sd, tol = 0.05)
  expect_equal(sd(y), y_sd, tol = 0.05)
  expect_equal(cor(x, y), r_xy, tol = .05)
})

test_that("add_random", {
  # start a data frame
  data1 <- add_random(school = 3)
  expect_equal(data1$school, paste0("school", 1:3))
  # nest classes in schools (2 classes per school)
  data2 <- add_random(data1, class = 2, .nested_in = "school")
  expect_equal(data2$class, paste0("class", 1:6))
  expect_equal(data2$school, rep(data1$school, each = 2))
  # nest students in each class (different n per class)
  n <- c(20, 24, 23, 21, 25, 24)
  data3 <- add_random(data2, student = n, .nested_in = "class")
  expect_equal(nrow(data3), sum(n))
  # cross each student with 10 questions
  data4 <- add_random(data3, question = 10)
  expect_equal(nrow(data4), sum(n)*10)
  expect_equal(data4$student, rep(data3$student, each = 10))
  
  # add crossed random factor to existing dataset
  data5 <- sim_design(within = 2, n = 3, long = TRUE, plot = FALSE)
  data6 <- add_random(data5, Q = 2)
  expect_equal(data6$Q, rep(c("Q1", "Q2"), 6))
  
  # compare nesting in 2 different factors
  data <- add_random(A = 2., B = 2)
  nested_in_A <- add_random(data, C = 2, .nested_in = "A")
  nested_in_B <- add_random(data, C = 2, .nested_in = "B")
  expect_false(all(nested_in_A$C == nested_in_B$C))
})

test_that("add_random ids", {
  # crossed random factors
  ids <- c("A", "B", "C")
  data1 <- add_random(school = ids)
  expect_equal(data1$school, ids)
  
  data1b <- add_random(school = ids, class = 3)
  check <- dplyr::tibble(
    school = rep(ids, each = 3),
    class = rep(paste0("class", 1:3), 3)
  )
  expect_equivalent(data1b, check)
  
  # nested random factors
  data2 <- add_random(data1, class = 2, .nested_in = "school")
  expect_equal(data2$class, paste0("class", 1:6))
  
  data2 <- add_random(data1, class = c(2, 3, 4), .nested_in = "school")
  expect_equal(data2$class, paste0("class", 1:9))
  
  data3 <- add_random(data1, 
                      class = list(
                        LETTERS[1:2],
                        LETTERS[3:5],
                        LETTERS[6:9]
                      ), 
                      .nested_in = "school")
  expect_equal(data3$class, LETTERS[1:9])
})

test_that("add_between", {
  base <- add_random(subj = 4, item = 2)
  
  data <- add_between(base, "subj", cond = c("A", "B"))
  cond <- rep(LETTERS[1:2], each = 2, times = 2) %>% factor()
  expect_equal(data$cond, cond)
  
  data <- add_between(base, "item", cond = c("A", "B"))
  cond <- rep(LETTERS[1:2], 4) %>% factor()
  expect_equal(data$cond, cond)
  
  # 2b2b
  data <- add_between(base, "subj", 
                      cond = c("A", "B"),
                      time = c("morning", "evening"))
  cond <- rep(LETTERS[1:2], each = 4) %>% factor()
  time <- rep(c("morning", "evening"), each = 2, times = 2) %>% 
    factor(levels = c("morning", "evening"))
  expect_equal(data$cond, cond)
  expect_equal(data$time, time)
  
  # shuffle
  set.seed(100)
  base <- add_random(subj = 100, item = 2)
  data <- add_between(base, "subj", time = c("morning", "evening"))
  data_shuffle <- add_between(base, "subj", time = c("morning", "evening"), 
                      .shuffle = TRUE)
  time <- rep(c("morning", "evening"), each = 2, times = 50) %>% 
    factor(levels = c("morning", "evening"))
  expect_equal(data$time, time)
  expect_false(all(data_shuffle$time == time))
  expect_equal(sum(data_shuffle$time == "morning"), 100)
  
  # prob proportion
  set.seed(100)
  mean_prob <- replicate(100, {
    data_prob <- add_between(base, "subj", time = c("morning", "evening"), 
                                .prob = c(.4, .6))
    mean(data_prob$time == "morning")
  }) %>% mean()
  expect_equal(mean_prob, .4, tol = .005)
  
  # exact prob
  for (n in c(0, 10, 20, 30, 100)) {
    data_prob <- add_between(base, "subj", time = c("morning", "evening"), 
                             .prob = c(n, 100-n))
    expect_equal(sum(data_prob$time == "morning"), n*2)
  }
  
  # multiple prob
  prob <- c(10, 20, 30, 40)
  data_prob2 <- add_between(base, "subj", 
                           cond = c("A", "B"),
                           time = c("morning", "evening"),
                           .prob = prob)
  n <- dplyr::count(data_prob2, cond, time)$n
  expect_equal(n, prob*2)
  
  expect_warning({data_prob3 <- 
    add_between(base, "subj", 
                cond = c("A", "B"),
                time = c("morning", "evening"),
                .prob = list(cond = c(10, 90),
                            time = c(90, 10)))})
  cond <- rep(c("A", "B"), c(10*2, 90*2)) %>% factor()
  time <- rep(c("morning", "evening"), c(90*2, 10*2)) %>% 
    factor(c("morning", "evening"))
  expect_equal(data_prob3$cond, cond)
  expect_equal(data_prob3$time, time)
  
  # independent joint probabilities
  set.seed(100)
  means <- replicate(100, {
    data_prob4 <- add_between(base, "subj", 
                            cond = c("A", "B"),
                            time = c("morning", "evening"),
                            .prob = list(cond = c(.3, .7),
                                         time = c(.3, .7)))
    list(
      cond = mean(data_prob4$cond == "A"),
      time = mean(data_prob4$time == "morning"),
      joint = mean(data_prob4$cond == "A" & 
                     data_prob4$time == "morning")
    )
  })
  cond <- means["cond", ] %>% unlist() %>% mean()
  time <- means["time", ] %>% unlist() %>% mean()
  joint <- means["joint", ] %>% unlist() %>% mean()
  expect_equal(cond, .3, tol = 0.01)
  expect_equal(time, .3, tol = 0.01)
  expect_equal(joint, .3*.3, tol = 0.01)
  
  ## numeric levels
  base <- add_random(subj = 6)
  x <- add_between(base, "subj", time = 3:5, letter = c("A", "B"))
  
  expect_true(is.numeric(x$time))
  expect_equal(x$time, c(3, 3, 4, 4, 5, 5))
  expect_true(is.factor(x$letter))
  expect_equal(levels(x$letter), c("A", "B"))
})

test_that("add_within", {
  base <- add_random(subj = 4, item = 2)
  
  data <- add_within(base, "subj", cond = c("A", "B"))
  cond <- rep(LETTERS[1:2], 4*2) %>% factor()
  expect_equal(data$cond, cond)
  
  data <- add_within(base, "item", cond = c("A", "B"))
  cond <- rep(LETTERS[1:2], 4*2) %>% factor()
  expect_equal(data$cond, cond)
  
  # 2b2b
  data <- add_within(base, "subj", 
                     cond = c("A", "B"),
                     time = c("morning", "evening"))
  cond <- rep(LETTERS[1:2], each = 2, times = 8) %>% factor()
  time <- rep(c("morning", "evening"), 16) %>% 
    factor(levels = c("morning", "evening"))
  expect_equal(data$cond, cond)
  expect_equal(data$time, time)
  
  ## numeric levels
  base <- add_random(subj = 2)
  x <- add_within(base, "subj", time = 3:4, letter = c("A", "B"))
  
  expect_true(is.numeric(x$time))
  expect_equal(x$time, c(3, 3, 4, 4, 3, 3, 4, 4))
  expect_true(is.factor(x$letter))
  expect_equal(levels(x$letter), c("A", "B"))
})

test_that("factor_char", {
  # should be numeric
  exp <- 1:5
  x <- 1:5
  obs <- factor_char(x)
  expect_equal(obs, exp)
  
  x <- as.character(1:5)
  obs <- factor_char(x)
  expect_equal(obs, exp)
  
  exp <- 0:1
  x <- 0:1
  obs <- factor_char(x)
  expect_equal(obs, exp)
  
  x <- c('0', '1')
  obs <- factor_char(x)
  expect_equal(obs, exp)
  
  x <- c(0L, 1L)
  obs <- factor_char(x)
  expect_equal(obs, exp)
  
  x <- c(0.0, 1.0)
  obs <- factor_char(x)
  expect_equal(obs, exp)
  
  # should be logical
  exp <- c(T, F)
  
  x <- c("TRUE", "FALSE")
  obs <- factor_char(x)
  expect_equal(obs, exp)
  
  x <- c(TRUE, FALSE)
  obs <- factor_char(x)
  expect_equal(obs, exp)
  
  x <- c("T", "F")
  obs <- factor_char(x)
  expect_equal(obs, exp)

  # should be factor
  x <- LETTERS[1:5]
  exp <- factor(x, x)
  
  obs <- factor_char(x)
  expect_equal(obs, exp)
  
  obs <- factor_char(exp)
  expect_equal(obs, exp)
})
