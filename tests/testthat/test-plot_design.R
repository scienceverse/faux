context("test-plot_design")

factor_maker <- function(factors) {
  if (is.numeric(factors)) factors <- LETTERS[factors]
  factors %>% stats::setNames(., .) %>% as.list() %>%
    lapply(function(x) {
      obj <- paste0("Level ", x, 1:2)
      nm <- paste0(x, 1:2)
      stats::setNames(obj, nm)
    })
}

vardesc_maker <- function(factors) {
  if (is.numeric(factors)) factors <- LETTERS[factors]
  
  factors %>% stats::setNames(paste("Factor", .), .)
}

user_opts <- faux_options("sep", "verbose", "plot", "connection")
on.exit(faux_options(user_opts))

faux_options(plot = FALSE)

# error ----
test_that("error", {
  df <- sim_design()
  attributes(df)$design <- NULL
  
  err <- "The data table must have a design attribute"
  expect_error(plot_design(df), err)
  expect_error(plot_design(iris), err)
  
  err <- "x must be a design list or a data frame"
  expect_error(plot_design(1), err)
  expect_error(plot_design("A"), err)
  expect_error(plot_design(matrix(1:100, 10)), err)
})

ggplot_classes <- c("ggplot2::ggplot",
                    "ggplot",
                    "ggplot2::gg",
                    "S7_object",
                    "gg")

# wide2long ----
test_that("wide2long", {
  set.seed(1)
  df <- sim_design()
  p1 <- plot_design(df)
  expect_equal(class(p1), ggplot_classes)
  
  set.seed(1)
  df <- sim_design(long = TRUE)
  p2 <- plot_design(df)
  expect_equal(class(p2), ggplot_classes)
  
  skip_on_cran()
  expect_true(all.equal.function(p1, p2))
})

# order ----
test_that("order", {
  des <- check_design(c(2,2,2,2,2,2))
  
  p <- plot_design(des, "W6", "W5", "W4", "W3", "W2", "W1")
  
  expect_equal(p$labels$x, "W5")
  expect_equal(p$labels$fill, "W6")
  expect_equal(p$labels$colour, "W6")
  expect_equal(p$facet$params$rows %>% names(), c("W4", "W3"))
  expect_equal(p$facet$params$cols %>% names(), c("W2", "W1"))
})

# subset ----
test_that("subset", {
  des <- check_design(c(2,2,2,2,2,2))
  p1 <- plot_design(des, "W1")
  expect_equal(p1$labels$x, "W1")
  # expect_equal(p1$labels$fill, "W1")
  # expect_equal(p1$labels$colour, "W1")
  expect_equal(p1$facet$params$rows %>% names(), NULL)
  expect_equal(p1$facet$params$cols %>% names(), NULL)
  
  p2 <- plot_design(des, "W2", "W1")
  expect_equal(p2$labels$x, "W1")
  expect_equal(p2$labels$fill, "W2")
  expect_equal(p2$labels$colour, "W2")
  expect_equal(p2$facet$params$rows %>% names(), NULL)
  expect_equal(p2$facet$params$cols %>% names(), NULL)
  
  p3 <- plot_design(des, "W3", "W2", "W1")
  expect_equal(p3$labels$x, "W2")
  expect_equal(p3$labels$fill, "W3")
  expect_equal(p3$labels$colour, "W3")
  expect_equal(p3$facet$params$rows %>% names(), "W1")
  expect_equal(p3$facet$params$cols %>% names(), character(0))
  
  p4 <- plot_design(des, "W4", "W3", "W2", "W1")
  expect_equal(p4$labels$x, "W3")
  expect_equal(p4$labels$fill, "W4")
  expect_equal(p4$labels$colour, "W4")
  expect_equal(p4$facet$params$rows %>% names(), "W2")
  expect_equal(p4$facet$params$cols %>% names(), "W1")
  
  p5 <- plot_design(des, "W5", "W4", "W3", "W2", "W1")
  expect_equal(p5$labels$x, "W4")
  expect_equal(p5$labels$fill, "W5")
  expect_equal(p5$labels$colour, "W5")
  expect_equal(p5$facet$params$rows %>% names(), "W3")
  expect_equal(p5$facet$params$cols %>% names(), c("W2", "W1"))
})

# from design ----
test_that("from design", {
  s0 <- check_design() %>% plot_design()
  s1 <- check_design(2) %>% plot_design()
  s2 <- check_design(c(2,2)) %>% plot_design()
  s3 <- check_design(c(2,2,2)) %>% plot_design()
  s4 <- check_design(c(2,2,2,2)) %>% plot_design()
  s5 <- check_design(c(2,2,2,2,2)) %>% plot_design()
  s6 <- check_design(c(2,2,2,2,2,2)) %>% plot_design()
  
  expect_equal(s0$labels$x, "value")
  expect_equal(s0$labels$y, "value")
  # expect_equal(s0$labels$fill, "fill")
  # expect_equal(s0$labels$colour, "colour")
  expect_equal(s0$facet$params, list())
  
  expect_equal(s1$labels$x, "W1")
  expect_equal(s1$labels$y, "value")
  # expect_equal(s1$labels$fill, "W1")
  # expect_equal(s1$labels$colour, "W1")
  expect_equal(s1$facet$params, list())
  
  expect_equal(s2$labels$x, "W2")
  expect_equal(s2$labels$y, "value")
  expect_equal(s2$labels$fill, "W1")
  expect_equal(s2$labels$colour, "W1")
  expect_equal(s2$facet$params, list())
  
  expect_equal(s3$labels$x, "W2")
  expect_equal(s3$labels$y, "value")
  expect_equal(s3$labels$fill, "W1")
  expect_equal(s3$labels$colour, "W1")
  expect_equal(s3$facet$params$rows %>% names(), c("W3"))
  expect_equal(s3$facet$params$cols %>% names(), character(0))
  
  expect_equal(s4$labels$x, "W2")
  expect_equal(s4$labels$y, "value")
  expect_equal(s4$labels$fill, "W1")
  expect_equal(s4$labels$colour, "W1")
  expect_equal(s4$facet$params$rows %>% names(), c("W3"))
  expect_equal(s4$facet$params$cols %>% names(), c("W4"))
  
  expect_equal(s5$labels$x, "W2")
  expect_equal(s5$labels$y, "value")
  expect_equal(s5$labels$fill, "W1")
  expect_equal(s5$labels$colour, "W1")
  expect_equal(s5$facet$params$rows %>% names(), c("W3"))
  expect_equal(s5$facet$params$cols %>% names(), c("W4", "W5"))
  
  expect_equal(s6$labels$x, "W2")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "W1")
  expect_equal(s6$labels$colour, "W1")
  expect_equal(s6$facet$params$rows %>% names(), c("W3", "W4"))
  expect_equal(s6$facet$params$cols %>% names(), c("W5", "W6"))
})

# from data ----
test_that("from data", {
  s0 <- sim_design() %>% plot_design()
  s1 <- sim_design(2) %>% plot_design()
  s2 <- sim_design(c(2,2)) %>% plot_design()
  s3 <- sim_design(c(2,2,2)) %>% plot_design()
  s4 <- sim_design(c(2,2,2,2)) %>% plot_design()
  s5 <- sim_design(c(2,2,2,2,2)) %>% plot_design()
  s6 <- sim_design(c(2,2,2,2,2,2)) %>% plot_design()
  
  expect_equal(s0$labels$x, "value")
  expect_equal(s0$labels$y, "value")
  # expect_equal(s0$labels$fill, "fill")
  # expect_equal(s0$labels$colour, "colour")
  expect_equal(s0$facet$params, list())
  
  expect_equal(s1$labels$x, "W1")
  expect_equal(s1$labels$y, "value")
  # expect_equal(s1$labels$fill, "W1")
  # expect_equal(s1$labels$colour, "W1")
  expect_equal(s1$facet$params, list())
  
  expect_equal(s2$labels$x, "W2")
  expect_equal(s2$labels$y, "value")
  expect_equal(s2$labels$fill, "W1")
  expect_equal(s2$labels$colour, "W1")
  expect_equal(s2$facet$params, list())
  
  expect_equal(s3$labels$x, "W2")
  expect_equal(s3$labels$y, "value")
  expect_equal(s3$labels$fill, "W1")
  expect_equal(s3$labels$colour, "W1")
  expect_equal(s3$facet$params$rows %>% names(), c("W3"))
  expect_equal(s3$facet$params$cols %>% names(), character(0))
  
  expect_equal(s4$labels$x, "W2")
  expect_equal(s4$labels$y, "value")
  expect_equal(s4$labels$fill, "W1")
  expect_equal(s4$labels$colour, "W1")
  expect_equal(s4$facet$params$rows %>% names(), c("W3"))
  expect_equal(s4$facet$params$cols %>% names(), c("W4"))
  
  expect_equal(s5$labels$x, "W2")
  expect_equal(s5$labels$y, "value")
  expect_equal(s5$labels$fill, "W1")
  expect_equal(s5$labels$colour, "W1")
  expect_equal(s5$facet$params$rows %>% names(), c("W3"))
  expect_equal(s5$facet$params$cols %>% names(), c("W4", "W5"))
  
  expect_equal(s6$labels$x, "W2")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "W1")
  expect_equal(s6$labels$colour, "W1")
  expect_equal(s6$facet$params$rows %>% names(), c("W3", "W4"))
  expect_equal(s6$facet$params$cols %>% names(), c("W5", "W6"))
})

# 2w ----
test_that("2w", {
  within <- list(time = c(day = "Tested during the day", 
                          night = "Tested at night"))
  between <- list()
  mu <- c(1,2)
  dv = "rt"
  id = "sub_id"
  d <- sim_design(within, between, mu = mu, dv = dv, id = id, long = TRUE)
  p <- plot_design(d)
  expect_equal(class(p), ggplot_classes)
  expect_equal(p$labels$x, "time")
  expect_equal(p$labels$y, "rt")
  # expect_equal(p$labels$fill, "time")
  # expect_equal(p$labels$colour, "time")
})

# 2w*2b ----
test_that("2w*2b", {
  within <- list(time = c("day", "night"))
  between <- list(pet = c("dog", "cat"))
  mu <- list(dog = 1:2, cat = 3:4)
  p <- check_design(within, between, mu = mu, plot = 0) %>% 
    plot_design()
  expect_equal(class(p), ggplot_classes)
  
  # axis labels
  within <- list(time = c(day = "Tested during the day", 
                          night = "Tested at night"))
  between <- list(pet = c(dog = "Has a dog", cat = "Has a cat"))
  mu <- list(
    dog = c(1,2),
    cat = c(3,4)
  )
  d <- sim_design(within, between, mu = mu, long = TRUE)
  p <- plot_design(d)
  expect_equal(class(p), ggplot_classes)
  expect_equal(p$labels$x, "pet")
  expect_equal(p$labels$y, "value")
  expect_equal(p$labels$fill, "time")
  expect_equal(p$labels$colour, "time")
})

# 2w*2w*2b ----
test_that("2w*2w*2b", {
  within <- list(time = c("day", "night"), condition = c("A", "B"))
  between <- list(pet = c("dog", "cat"))
  mu <- list(dog = 1:4, cat = 2:5)
  p <- check_design(within, between, mu = mu) %>%
    plot_design()
  expect_equal(class(p), ggplot_classes)
  expect_equal(p$labels$x, "condition")
  expect_equal(p$labels$y, "value")
  expect_equal(p$labels$fill, "time")
  expect_equal(p$labels$colour, "time")
})

# 2w*2w*2b*2b ----
test_that("2w*2w*2b*2b", {
  within <- list(pet = c("ferret", "dog", "cat"), condition = c("A", "B"))
  between <- list(time = c("night", "day"), age = c("old", "young"))
  mu <- list(night_old = 1:6, 
             day_old = 2:7,
             night_young = 3:8, 
             day_young = 4:9)
  design <- check_design(within, between, mu = mu)
  p <- plot_design(design)
  expect_equal(class(p), ggplot_classes)
  expect_equal(names(p$facet$params$rows), "time")
  expect_equal(names(p$facet$params$cols), "age")
  expect_equal(p$labels$x, "condition")
  expect_equal(p$labels$y, "value")
  expect_equal(p$labels$fill, "pet")
  expect_equal(p$labels$colour, "pet")
})

# geoms ----
test_that("geoms", {
  dat <- sim_design(c(2,2,2,2), n = 25, sd = 5, 
                    mu = 1:16)
  
  default <- plot_design(dat)
  manual <- plot_design(dat, geoms = c("violin", "box"))
  #expect_equal(default, manual)
  
  v  <- plot_design(dat, geoms = "violin")
  b  <- plot_design(dat, geoms = "box")
  sd <- plot_design(dat, geoms = "pointrangeSD")
  se <- plot_design(dat, geoms = "pointrangeSE")
  j  <- plot_design(dat, geoms = "jitter")
  
  v_class  <- v$layers[[1]]$geom  %>% class()
  b_class  <- b$layers[[1]]$geom  %>% class()
  sd_class <- sd$layers[[1]]$geom %>% class()
  se_class <- se$layers[[1]]$geom %>% class()
  j_class  <- j$layers[[1]]$geom  %>% class()
  
  expect_equal(v_class[[1]], "GeomViolin")
  expect_equal(b_class[[1]], "GeomBoxplot")
  expect_equal(sd_class[[1]], "GeomPointrange")
  expect_equal(se_class[[1]], "GeomPointrange")
  expect_equal(j_class[[1]], "GeomPoint")
  
  v_sd <- plot_design(dat, geoms = c("violin", "pointrangeSD"))
  v_sd1 <- v_sd$layers[[1]]$geom  %>% class()
  v_sd2 <- v_sd$layers[[2]]$geom  %>% class()
  expect_equal(v_sd1[1], "GeomViolin")
  expect_equal(v_sd2[1], "GeomPointrange")
  
  # only does one of pointrange - should default to SD
  se_sd <- plot_design(dat, geoms = c("pointrangeSE", "pointrangeSD"))
  sd_se <- plot_design(dat, geoms = c("pointrangeSD", "pointrangeSE"))
  expect_equal(length(se_sd$layers), 1)
  expect_equal(length(sd_se$layers), 1)
  
  # get rid of plot_env$geoms to compare
  se_sd$plot_env$geoms <- NULL
  sd_se$plot_env$geoms <- NULL
  sd$plot_env$geoms <- NULL
  
  skip_on_cran()
  expect_true(all.equal.function(se_sd, sd_se))
  expect_true(all.equal.function(sd, sd_se))
})

# S3 functions ----
test_that("S3 functions", {
  des <- check_design()
  dat <- sim_design()
  p_des <- plot(des)$layers[[1]]$geom %>% class()
  p_dat <- plot(dat)$layers[[1]]$geom %>% class()
  
  expect_equal(p_des[1], "GeomPointrange")
  expect_equal(p_dat[1], "GeomViolin")
  
  p_des <- plot(des, geoms=c("jitter", "violin"))$layers[[1]]$geom %>% class()
  p_dat <- plot(dat, geoms=c("jitter", "violin"))$layers[[1]]$geom %>% class()
  
  expect_equal(p_des[1], "GeomPoint")
  expect_equal(p_dat[1], "GeomPoint")
})

# reps ----
test_that("reps", {
  
  data <- sim_design(2, 2, rep = 3)
  p <- plot_design(data)
  
  expect_equal(p$facet$params, list())
})

# vardesc ----
test_that("vardesc", {
  between <- factor_maker("B")
  within <- factor_maker("W")
  vardesc <- vardesc_maker(c("B", "W"))
  
  # 1 factor
  p <- check_design(within, vardesc = vardesc) %>% plot_design()
  expect_equal(p$labels$x, vardesc[["W"]])
  
  # 2 factors
  p <- check_design(within, between, vardesc = vardesc) %>% plot_design()
  expect_equal(p$labels$x, vardesc[["B"]])
  expect_equal(p$labels$colour, vardesc[["W"]])
  expect_equal(p$labels$fill, vardesc[["W"]])
  
  # 6 factors
  within <- factor_maker(1:6)
  vardesc <- vardesc_maker(1:6)
  p <- check_design(within, vardesc = vardesc) %>% plot_design()
  expect_equal(p$labels$x, vardesc[["B"]])
  expect_equal(p$labels$colour, vardesc[["A"]])
  expect_equal(p$labels$fill, vardesc[["A"]])
  
  expect_equal(names(p$facet$params$rows), c("C", "D"))
  expect_equal(names(p$facet$params$cols), c("E", "F"))
  
  # check custom labeller
  design <- check_design(within, vardesc = vardesc)
  p_value_st <- plot_design(design, labeller = "label_value")
  p_both_st  <- plot_design(design, labeller = "label_both")
  p_value_fu <- plot_design(design, labeller = label_value)
  p_both_fu  <- plot_design(design, labeller = label_both)
  
  df <- c(design$within, design$between) %>%
    `[`(LETTERS[3:6]) %>%
    lapply(unlist) %>%
    as.data.frame()
  # or get from plot
  # df <- ggplot_build(p_both_st)$layout$layout[LETTERS[3:6]]
  
  both_labs <- LETTERS[3:6] %>%
    lapply(function(x) paste0("Factor ", x, ": Level ", x , 1:2)) %>%
    stats::setNames(LETTERS[3:6])
  value_labs <- LETTERS[3:6] %>%
    lapply(function(x) paste0("Level ", x , 1:2)) %>%
    stats::setNames(LETTERS[3:6])
  
  expect_equal(p_value_st$facet$params$labeller(df), value_labs)
  expect_equal(p_both_st$facet$params$labeller(df), both_labs)
  expect_equal(p_value_fu$facet$params$labeller(df), value_labs)
  expect_equal(p_both_fu$facet$params$labeller(df), both_labs)
})

faux_options(plot = TRUE)