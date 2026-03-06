test_that("exists", {
  expect_true(is.function(faux::add_dv))
  expect_no_error(helplist <- help(add_dv, faux))
})

test_that("errors", {
  expect_error(add_dv(bad_arg))
})

test_that("defaults", {
  df <- add_random(id = 12) |>
    add_between(cond = c("ctl", "exp"),
                age = c("young", "middle", "old")
    )
  
  df1 <- add_dv(df)
  expect_equal(names(df1), c("id", "cond", "age", "dv"))
  expect_true(all(!is.na(df1$dv)))
  
  df2 <- add_dv(df, between = list(cond = "ctl"))
  x <- df2$cond == "ctl"
  expect_false(df2$dv[x] |> is.na() |> any())
  expect_true(df2$dv[!x] |> is.na() |> all())
  
  df3 <- add_dv(df, between = list(cond = "ctl", age = "young"))
  x <- df3$cond == "ctl" & df3$age == "young"
  expect_false(df3$dv[x] |> is.na() |> any())
  expect_true(df3$dv[!x] |> is.na() |> all())
  
  df4 <- df |>
    add_dv(between = list(age = "young"), mean = 0) |>
    add_dv(between = list(age = "middle"), mean = 10) |>
    add_dv(between = list(age = "old"), mean = 20) 
  y <- mean(df4$dv[df4$age == "young"])
  m <- mean(df4$dv[df4$age == "middle"])
  o <- mean(df4$dv[df4$age == "old"])
  expect_true(y < m)
  expect_true(m < o)
})

test_that("correlated DVs", {
  df <- add_random(id = 12) |>
    add_between(cond = c("ctl", "exp"),
                age = c("young", "middle", "old")
    ) |>
    add_within(time = c(pre = "Pre-Test", post = "Post-Test"))
  
  design <- get_design(df)
  
  df1 <- add_dv(df, r = 0.9)
  expect_equal(names(df1)[5], "dv")
  df1_wide <- long2wide(df1)
  expect_true(cor(df1_wide$pre, df1_wide$post) > .5)
  
  df2 <- add_dv(df, mean = c(0, 10))
  pre_m <- mean(df2$dv[df2$time == "pre"])
  post_m <- mean(df2$dv[df2$time == "post"])
  expect_true(pre_m < post_m)
})

test_that("non-normal", {
  df <- add_random(id = 12) |>
    add_between(cond = c("ctl", "exp"),
                age = c("young", "middle", "old")
    )
  
  df_pois <- add_dv(df, 
                    dist = "pois", 
                    params = list(dv = list(lambda = 4)))
  expect_true(all(df_pois$dv %in% 0:20))
  
  df_binom <- add_dv(df, 
                     dist = "binom", 
                     params = list(dv = list(size = 1, prob = 0.5)))
  expect_true(all(df_binom$dv %in% 0:1))
})


test_that("design", {
  df <- add_random(id = 10)
  d <- get_design(df)
  expect_equal(d$n, 10)
  expect_equal(d$id, list(id = "id"))
  
  df <- add_random(id = 20) |>
    add_between(a = c(a1 = "A1", a2 = "A2"),
                b = c(b1 = "B1", b2 = "B2"))
  d <- get_design(df)
  expect_equal(d$n, 20)
  expect_equal(d$id, list(id = "id"))
  exp <- list(a = list(a1 = "A1", a2 = "A2"),
              b = list(b1 = "B1", b2 = "B2"))
  expect_equal(d$between, exp)
  
  df <- add_random(id = 20) |>
    add_between(b = c(b1 = "B1", b2 = "B2")) |>
    add_within(w = c(w1 = "W1", w2 = "W2"))
  d <- get_design(df)
  expect_equal(d$n, 20)
  expect_equal(d$id, list(id = "id"))
  expect_equal(d$between, list(b = list(b1 = "B1", b2 = "B2")))
  expect_equal(d$within, list(w = list(w1 = "W1", w2 = "W2")))
})




# test_that("errors", {
#   dat <- sim_design(2, 2, long = TRUE)
#   expect_error(add_dv(dat, y ~ x), regexp = ": x$")
#   expect_error(add_dv(dat, y ~ age), regexp = ": age$")
#   expect_error(add_dv(dat, y ~ x*age), regexp = ": x, age$")
# 
#   expect_warning(add_dv(dat, W1 ~ B1), "The column W1 will be overwritten")
# })
# 
# test_that("basic", {
#   dat <- add_random(rater = 5)
# 
#   # default
#   dat1 <- add_dv(dat)
#   expect_equal(c("rater", "y"), colnames(dat1))
#   expect_equal(rep(0, 5), dat1$y)
# 
#   # change intercept
#   dat1 <- add_dv(dat, intercept = 10)
#   expect_equal(c("rater", "y"), colnames(dat1))
#   expect_equal(rep(10, 5), dat1$y)
# 
#   # change dv name using formula
#   dat2 <- add_dv(dat, dv ~ 1)
#   expect_equal(c("rater", "dv"), colnames(dat2))
#   expect_equal(rep(0, 5), dat2$dv)
# 
#   # change dv name using text
#   dat3 <- add_dv(dat, "dv ~ 1")
#   expect_equal(c("rater", "dv"), colnames(dat3))
#   expect_equal(rep(0, 5), dat3$dv)
# })
# 
# test_that("categorical IVs", {
#   dat <- add_random(rater = 6) |>
#     add_between("rater", x = c("A", "B"))
# 
#   dat1 <- add_dv(dat, y ~ x, list(x = 1))
#   expect_equal(dat1$y, rep(0:1, 3))
# })
