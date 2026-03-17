#' Generate a cross-classified sample 
#' 
#' Makes a basic cross-classified design with random intercepts for subjects and items. See \code{vignette("sim_mixed", package = "faux")} for examples and details.
#'
#' @param sub_n the number of subjects
#' @param item_n the number of items
#' @param grand_i the grand intercept (overall mean)
#' @param sub_sd the SD of subject random intercepts (or a sub_n-length named vector of random intercepts for each subject)
#' @param item_sd the SD of item random intercepts (or an item_n-length named vector of random intercepts for each item)
#' @param error_sd the SD of the error term
#' @param empirical Should the returned data have these exact parameters? (versus be sampled from a population with these parameters)
#' @param seed DEPRECATED use set.seed() instead before running this function
#'
#' @return a tbl 
#' @export
#'
#' @examples
#' 
#' sim_mixed_cc(10, 10)
sim_mixed_cc <- function(sub_n = 100, item_n = 20, grand_i = 0, 
                         sub_sd = 1, item_sd = 1, error_sd = 1, 
                         empirical = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() instead")
  #   # reinstate system seed after simulation
  #   gs <- global_seed(); on.exit(global_seed(gs))
  #   set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  }
  
  # sample subject random intercepts----
  if (length(sub_sd) == sub_n) {
    # use exact subject intercepts and names
    sub_id <- names(sub_sd) # if sub_sd is a named vector or list
    if (is.null(sub_id)) sub_id <- make_id(1:sub_n, "S")
    
    new_sub <- data.frame(
      sub_id = sub_id,
      sub_i = sub_sd %>% unname()
    )
  } else if (length(sub_sd) == 1) {
    # sample new subjects and intercepts
    new_sub <- data.frame(
      sub_id = make_id(1:sub_n, "S"),
      sub_i = rnorm_multi(sub_n, 1, 0, sub_sd, 
                          empirical = empirical, 
                          as.matrix = TRUE)[,1]
    )
  }
  
  
  # sample item random intercepts ----
  if (length(item_sd) == item_n) {
    # use exact item intercepts and names
    item_id <- names(item_sd) # if item_sd is a named vector or list
    if (is.null(item_id)) item_id <- make_id(1:item_n, "I")
    
    new_item <- data.frame(
      item_id = item_id,
      item_i = item_sd %>% unname()
    )
  } else if (length(item_sd) == 1) {
    # sample new items and intercepts
    new_item <- data.frame(
      item_id = make_id(1:item_n, "I"),
      item_i = rnorm_multi(item_n, 1, 0, item_sd, 
                           empirical = empirical, 
                           as.matrix = TRUE)[,1]
    )
  }
  
  ids <- expand.grid(item_id = new_item$item_id,
                    sub_id = new_sub$sub_id)
  ids_sub <- merge(ids, new_sub, by = "sub_id", sort = FALSE)
  new_obs <- merge(ids_sub, new_item, by= "item_id", sort = FALSE)
  new_obs <- new_obs[order(new_obs$sub_id, new_obs$item_id), 
                     c("sub_id", "item_id", "sub_i", "item_i")]
  new_obs$grand_i <- grand_i
  new_obs$err <- stats::rnorm(nrow(new_obs), 0, error_sd)
  new_obs$y <- new_obs$grand_i + new_obs$sub_i + new_obs$item_i + new_obs$err
  colorder <- c("sub_id", "item_id", "y", "grand_i", "sub_i", "item_i", "err")
  new_obs <- new_obs[colorder]
  
  new_obs
}
