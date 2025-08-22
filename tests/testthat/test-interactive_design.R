context("test-interactive_design")

test_that("default", {
  des2 <- check_design(id = "id", dv = "y", plot = FALSE)
  
  # set up interactive answers
  f <- file()
  faux_options(connection = f)
  on.exit({
    faux_options(connection = stdin()) # reset connection
    close(f) # close the file
  })
  
  lines <- c("0", "0", "y", "id", "100", "0", "1")
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  capture_output_lines({des <- interactive_design()})

  # check match
  expect_equal(des$within,  des2$within)
  expect_equal(des$between, des2$between)
  expect_equal(des$id, des2$id)
  expect_equal(des$dv, des2$dv)
  expect_equal(des$n,  des2$n)
  expect_equal(des$mu, des2$mu)
  expect_equal(des$sd, des2$sd)
  expect_equal(des$r,  des2$r)
})

test_that("default", {
  # set up canonial design
  within <- list(A = c("A1", "A2", "A3"))
  between <- list(B = c("B1", "B2"))
  n <- 10
  mu <- list(
    B1 = 1:3,
    B2 = 4:6
  )
  sd <- 1
  r <- list(
    B1 = .5,
    B2 = c(.4, .5, .6)
  )
  des2 <- check_design(within, between, n, mu, sd, r, dv = "rt", id = "subid", plot = FALSE)
  
  # set up interactive answers
  f <- file()
  faux_options(connection = f)
  on.exit({
    faux_options(connection = stdin()) # reset connection
    close(f) # close the file
  })
  
  lines <- c(
    "1", "A", "3", "A1", "A2", "A3", "1", "B", "2", "B1", "B2", 
    "rt", "subid", "10", "1,2,3", "4,5,6", "1", "1", ".5", ".4, .5, .6"
  )
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  capture_output_lines({des <- interactive_design()})
  
  # check match
  expect_equal(des$within,  des2$within)
  expect_equal(des$between, des2$between)
  expect_equal(des$id, des2$id)
  expect_equal(des$dv, des2$dv)
  expect_equal(des$n,  des2$n)
  expect_equal(des$mu, des2$mu)
  expect_equal(des$sd, des2$sd)
  expect_equal(des$r,  des2$r)
})

test_that("get it wrong", {
  # set up canonial design
  within <- list(A = c("A1", "A2", "A3"))
  between <- list(B = c("B1", "B2"))
  n <- 10
  mu <- list(
    B1 = 1:3,
    B2 = 4:6
  )
  sd <- 1
  r <- list(
    B1 = .5,
    B2 = c(.4, .5, .6)
  )
  des2 <- check_design(within, between, n, mu, sd, r, dv = "rt", id = "subid", plot = FALSE)
  
  # set up interactive answers
  f <- file()
  faux_options(connection = f)
  on.exit({
    faux_options(connection = stdin()) # reset connection
    close(f) # close the file
  })
  
  lines <- c(
    "X", "1", "", "A", "X", "3", "", "A1", "", "A2", "", "A3", "X", "1", 
    "", "B", "X", "2", "", "B1", "", "B2", 
    "", "rt", "", "subid", "X", "10", "X", "1,2,3", "X", "4,5,6", 
    "X", "1", "X", "1", "X", ".5", "X", ".4, .5, .6"
  )
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  capture_output_lines({des <- interactive_design(plot = FALSE)})
  
  # check match
  expect_equal(des$within,  des2$within)
  expect_equal(des$between, des2$between)
  expect_equal(des$id, des2$id)
  expect_equal(des$dv, des2$dv)
  expect_equal(des$n,  des2$n)
  expect_equal(des$mu, des2$mu)
  expect_equal(des$sd, des2$sd)
  expect_equal(des$r,  des2$r)
})

test_that("issue106", {
  # https://github.com/scienceverse/faux/issues/106
  
  
  # set up canonical design
  within <- list(headline = c("TRUTH", "FAKE"),
                 image = c("PIC", "NOPIC"))
  between <- list()
  dv <- "accuracy"
  id <- "id"
  n <- 211
  mu <- c(2.66, 2.52, 2.53, 2.41)
  sd <- c(2.66, 2.52, 2.66, 2.52)
  r <- c(.01, .02, .03, .04, .05, .06)

  des2 <- check_design(within, between, n, mu, sd, r, dv, id, plot = FALSE)
  
  # set up interactive answers
  f <- file()
  faux_options(connection = f)
  on.exit({
    faux_options(connection = stdin()) # reset connection
    close(f) # close the file
  })
  
  lines <- c(
    2, "headline", 2, "TRUTH", "FAKE", 
    "image", 2, "PIC", "NOPIC", 
    0, "accuracy", "id", 211, 
    "2.66, 2.52, 2.53, 2.41", 
    " 2.66, 2.52 ",
    ".01, .02, .03, .04, .05, .06"
  )
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  capture_output_lines({des <- interactive_design(plot = FALSE)})
  
  # check match
  expect_equal(des,  des2)
})
