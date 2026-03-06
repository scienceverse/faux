test_that("rep_if", {
  x <- list()
  obs <- rep_if(x, 1)
  exp <- list()
  expect_equal(obs, exp)
  
  x <- list(NULL, 2)
  obs <- rep_if(x, 1)
  exp <- list(1, 2)
  expect_equal(obs, exp)
  
  x <- list(NA, 2)
  obs <- rep_if(x, 1, NA)
  exp <- list(1, 2)
  expect_equal(obs, exp)
  
  x <- list("", 2)
  obs <- rep_if(x, 1, "")
  exp <- list(1, 2)
  expect_equal(obs, exp)
  
  x <- list("", 2, NA)
  obs <- rep_if(x, 1, c("", NA))
  exp <- list(1, 2, 1)
  expect_equal(obs, exp)
  
  x <- list(1, NULL, 3, NULL, NULL)
  y <- 1:5
  obs <- rep_if(x, y)
  exp <- as.list(1:5)
  expect_equal(obs, exp)
  
  x <- c("", "B", "X")
  y <- LETTERS[1:3]
  obs <- rep_if(x, y, c("", "X"))
  exp <- LETTERS[1:3]
  expect_equal(obs, exp)
})

