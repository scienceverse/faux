test_that("loading", {
  # # set to something wierd to make sure they're reset later
  # faux_options(connection = "a", 
  #              sep = "b", 
  #              verbose = "c", 
  #              plot = "d")
  # 
  # detach("package:faux", unload=TRUE)
  #expect_error(sim_design(), 'could not find function "sim_design"', fixed = TRUE)
#   options(faux.connection = NULL)
#   options(faux.sep = NULL)
#   options(faux.verbose = NULL)
#   options(faux.plot = NULL)
#   expect_equal(options("faux.connection")[[1]], NULL)
#   expect_equal(options("faux.sep")[[1]], NULL)
#   expect_equal(options("faux.verbose")[[1]], NULL)
#   expect_equal(options("faux.plot")[[1]], NULL)
#   
#   startup <- "************
# Welcome to faux. For support and examples visit:
# http://scienceverse.github.io/faux/
# - Get and set global package options with: faux_options()
# ************"
#   
#   expect_message(library("faux"), startup, fixed = TRUE)
  
  expect_equal(options("faux.connection")[[1]], stdin())
  expect_equal(options("faux.sep")[[1]], "_")
  expect_equal(options("faux.verbose")[[1]], TRUE)
  expect_equal(options("faux.plot")[[1]], TRUE)
})
