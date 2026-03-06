#' Generate a mixed design from existing data
#'
#' `sim_mixed_df()` produces a data table with the same distributions of 
#' by-subject and by-item random intercepts as an existing data table.
#'
#' @param data the existing tbl
#' @param sub_n the number of subjects to simulate (if NULL, returns data for the same subjects)
#' @param item_n the number of items to simulate (if NULL, returns data for the same items)
#' @param dv the column name or index containing the DV
#' @param sub_id the column name or index for the subject IDs
#' @param item_id the column name or index for the item IDs
#' 
#' @return a tbl
#' @examples
#' \donttest{sim_mixed_df(faceratings, 10, 10, "rating", "rater_id", "face_id")}
#' @export

sim_mixed_df <- function(data, sub_n = NULL, item_n = NULL, 
                        dv = "y", sub_id = "sub_id", item_id = "item_id") {
  
  params <- check_mixed_design(data, dv, sub_id, item_id)
  
  # get exact intercepts if sub_n or item_n is NULL
  if (is.null(item_n)) {
    if (is.numeric(item_id)) item_id <- names(data)[item_id]
    params$item_sd <- params$random_effects[[item_id]][1] %>% as.matrix()
    item_n <- length(params$item_sd)
  }
  if (is.null(sub_n)) {
    if (is.numeric(sub_id)) sub_id <- names(data)[sub_id]
    params$sub_sd <- params$random_effects[[sub_id]][1] %>% as.matrix()
    sub_n <- length(params$sub_sd)
  }
  
  new_obs <- sim_mixed_cc(sub_n, item_n, params$grand_i, 
                          params$sub_sd, params$item_sd, params$error_sd)
  
  new_obs
}
