test_that("sim_design", {  
  between <- list(
    B = c(B1 = "Level 1B", B2 = "Level 2B")
  )
  within <- list(
    W = c(W1 = "Level 1W", W2 = "Level 2W")
  )
  
  vardesc <- list(B = "Between-Subject Factor",
                  W = "Within-Subject Factor")
  
  # wide ----
  dat <- sim_design(within, between, vardesc = vardesc, plot = FALSE)
  design <- get_design(dat)
  expect_mapequal(design$vardesc, vardesc)
  
  obsID <- attr(dat$id, "label")
  expect_equal(obsID, "id")
  
  obsB <- attr(dat$B, "label")
  expect_equal(obsB, vardesc$B)
  
  obsW1 <- attr(dat$W1, "label")
  expect_equal(obsW1, within$W[['W1']])
  
  obsW2 <- attr(dat$W2, "label")
  expect_equal(obsW2, within$W[['W2']])

  # long ----
  dat <- sim_design(within, between, 
                    vardesc = vardesc, 
                    long = TRUE, plot = FALSE)
  design <- get_design(dat)
  expect_mapequal(design$vardesc, vardesc)
  
  obsID <- attr(dat$id, "label")
  expect_equal(obsID, "id")
  
  obsY <- attr(dat$y, "label")
  expect_equal(obsY, "value")
  
  obsB <- attr(dat$B, "label")
  expect_equal(obsB, vardesc$B)
  
  obsW <- attr(dat$W, "label")
  expect_equal(obsW, vardesc$W)
})

test_that("sim_design wide 2-within", {  
  within <- list(
    A = c(A1 = "Level 1A", A2 = "Level 2A"),
    B = c(B1 = "Level 1B", B2 = "Level 2B")
  )
  
  vardesc <- list(A = "First Factor",
                  B = "Second Factor")
  
  dat <- sim_design(within, vardesc = vardesc, plot = FALSE)
  
  obsID <- attr(dat$id, "label")
  expect_equal(obsID, "id")
  
  obs <- attr(dat$A1_B1, "label")
  expect_equal(obs, "Level 1A:Level 1B")
  
  obs <- attr(dat$A1_B2, "label")
  expect_equal(obs, "Level 1A:Level 2B")
  
  obs <- attr(dat$A2_B1, "label")
  expect_equal(obs, "Level 2A:Level 1B")
  
  obs <- attr(dat$A2_B2, "label")
  expect_equal(obs, "Level 2A:Level 2B")
})

test_that("sim_design no within", {  
  dat <- sim_design(id = c(stim_id = "Stimulus ID"),
                    dv = c(age = "Age"), 
                    plot = FALSE)
  
  obsID <- attr(dat$stim_id, "label")
  expect_equal(obsID, "Stimulus ID")
  
  obsY <- attr(dat$age, "label")
  expect_equal(obsY, "Age")
})

test_that("sim_design rep", {  
  # nested ----
  dat <- sim_design(n = 20, rep = 2, plot = FALSE)
  
  obsID <- attr(dat$data[[1]]$id, "label")
  expect_equal(obsID, "id")
  
  obsY <- attr(dat$data[[1]]$y, "label")
  expect_equal(obsY, "value")
  
  expect_equal(attr(dat$data, "label"), "data")
  expect_equal(attr(dat$rep, "label"), "replicate index")
  
  # unnested ----
  dat <- sim_design(n = 20, rep = 2, nested = FALSE, plot = FALSE)
  
  expect_equal(attr(dat$id,  "label"), "id")
  expect_equal(attr(dat$y,   "label"), "value")
  expect_equal(attr(dat$rep, "label"), "replicate index")
})

test_that("long2wide/wide2long", {
  between <- list(
    B = c(B1 = "Level 1B", B2 = "Level 2B")
  )
  within <- list(
    W = c(W1 = "Level 1W", W2 = "Level 2W")
  )
  vardesc <- list(B = "Between-Subject Factor",
                  W = "Within-Subject Factor")
  
  # wide2long
  dat_w <- sim_design(within, between, vardesc = vardesc, 
                      long = FALSE, plot = FALSE)
  l <- wide2long(dat_w)
  
  obsB <- attr(l$B, "label")
  expect_equal(obsB, vardesc$B)
  
  obsW <- attr(l$W, "label")
  expect_equal(obsW, vardesc$W)
  
  # long2wide
  dat_l <- sim_design(within, between, vardesc = vardesc, 
                      long = TRUE, plot = FALSE)
  w <- long2wide(dat_l)
  
  obsB <- attr(w$B, "label")
  expect_equal(obsB, vardesc$B)
  
  obsW1 <- attr(w$W1, "label")
  expect_equal(obsW1, within$W[['W1']])
  
  obsW2 <- attr(w$W2, "label")
  expect_equal(obsW2, within$W[['W2']])
  
})
