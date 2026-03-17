context("long2wide")

faux_options(plot = FALSE)

# 2w ----
test_that("2w", {
  df_long <- sim_design(within = 2, long = TRUE)
  df_wide <- long2wide(df_long, "W1", c(), "y", "id")
  
  expect_equal(nrow(df_wide), 100)
  expect_equal(ncol(df_wide), 3)
  expect_equal(colnames(df_wide), c("id", "W1a", "W1b"))
})

# 2b ----
test_that("2b", {
  df_long <- sim_design(between = 2, long = TRUE)
  df_wide <- long2wide(df_long, c(), "B1", "y", "id")
  
  expect_equal(nrow(df_wide), 200)
  expect_equal(ncol(df_wide), 3)
  expect_equal(colnames(df_wide), c("id", "B1", "y"))
  expect_equivalent(df_long, df_wide)
})

# 2w*2b ----
test_that("2w*2b", {
  df_long <- sim_design(2, 2, long = TRUE)
  df_wide <- long2wide(df_long, "W1", "B1", "y", "id")
  
  expect_equal(names(df_wide), c("id", "B1", "W1a", "W1b"))
  expect_equal(nrow(df_wide), 200)
})

# named arguments ----
test_that("named arguments", {
  df_long <- sim_design(2, 2, long = TRUE)
  df_wide <- long2wide(dv = "y", id = "id", data = df_long, between = "B1", within = "W1")
  
  expect_equal(names(df_wide), c("id", "B1", "W1a", "W1b"))
  expect_equal(nrow(df_wide), 200)
})

# 2w*2w*2b*2b ----
test_that("2w*2w*2b*2b", {
  df_long <- sim_design(c(2, 2), c(2, 2), long = TRUE)
  df_wide <- long2wide(df_long, c("W1", "W2"), c("B1","B2"), "y", "id")
  
  expect_equal(names(df_wide), c("id", "B1", "B2", "W1a_W2a", "W1a_W2b", "W1b_W2a", "W1b_W2b"))
  expect_equal(nrow(df_wide), 400)
})


# iris ----
test_that("iris", {
  iris_long <- iris %>%
    dplyr::mutate(id = make_id(nrow(.), "I")) %>%
    tidyr::gather(var, y, Sepal.Length:Petal.Width) %>%
    tidyr::separate(var, c("Feature", "Measure"))
  
  iris_wide <- long2wide(iris_long, within = c("Feature", "Measure"), 
                         between = "Species", dv = "y", id = "id")
  inames <- c("id", "Species", "Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width")
  expect_equal(names(iris_wide), inames)
  expect_equal(nrow(iris_wide), 150)
  
  long <- check_sim_stats(iris_long, within = c("Feature", "Measure"), 
                          between = "Species", dv = "y", id = "id")
  
  wide <- check_sim_stats(iris, between = "Species")
  
  
  expect_equal(nrow(long), nrow(wide))
})

# wide2long ----
test_that("wide2long", {
  long_iris <- wide2long(
    iris,
    within_factors = c("feature", "dimension"),
    within_cols = c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width"),
    dv = "value",
    id = "ID",
    sep = "\\."
  )
  
  expect_equal(nrow(long_iris), 600)
  expect_equal(names(long_iris), c("ID", "Species", "feature", "dimension", "value"))
  expect_true(is.factor(long_iris$feature))
  expect_true(is.factor(long_iris$dimension))
  
  long_iris <- wide2long(iris, c("Feature", "Measure"), 1:4, sep = "\\.")
  expect_equal(nrow(long_iris), 600)
  expect_equal(names(long_iris), c("id", "Species", "Feature", "Measure", "y"))
})

# from design ----
test_that("from design", {
  w <- sim_design(c(2,2), c(2,2), plot = FALSE)
  w2 <- wide2long(w)
  w3 <- long2wide(w2)
  expect_equal(class(w), c("faux", "data.frame"))
  expect_equal(class(w2), c("faux", "data.frame"))
  expect_equal(class(w3), c("faux", "data.frame"))
  
  expect_equal(names(w2), c("id", "B1", "B2", "W1", "W2", "y"))
  #expect_equal(w, w3)

  l <- sim_design(c(2,2), c(2,2), long = TRUE)
  l2 <- long2wide(l)
  expect_equal(class(l), c("faux", "data.frame"))
  expect_equal(class(l2), c("faux", "data.frame"))
  expect_equal(names(l2), c("id", "B1", "B2", "W1a_W2a", "W1a_W2b", "W1b_W2a", "W1b_W2b"))

  # from data not made by faux and grouped
  data <- fr4 %>%
    dplyr::group_by(rater_id, rater_sex, face_eth) %>%
    dplyr::summarise(rating = mean(rating), .groups = "drop")
  
  dwide <- long2wide(data, within = "face_eth", between = "rater_sex", dv = "rating", id = "rater_id")
  expect_equal(names(dwide), c("rater_id", "rater_sex", "black", "east_asian", "west_asian", "white"))
  
  # same data made by faux
  within <- list(face_eth = c("black", "east_asian", "west_asian", "white"))
  between <- list(rater_sex = c("male", "female"))
  dv <- "rating"
  id <- "rater_id"
  data2 <- sim_design(within, between, n = 12,
                      dv = dv, id = id, long = TRUE, sep = "~")
  
  dwide2 <- long2wide(data2, within = "face_eth", between = "rater_sex", dv = "rating", id = "rater_id")
  expect_equal(names(dwide2), c("rater_id", "rater_sex", "black", "east_asian", "west_asian", "white"))
})


faux_options(plot = TRUE)