#' Attractiveness ratings of faces
#'
#' A dataset containing attractiveness ratings (on a 1-7 scale from "much less attractiveness than average" to "much more attractive than average") for the neutral front faces from 2513 people (ages 17-90) 
#'
#' @format A data frame with 256326 rows and 9 variables:
#' \describe{
#'   \item{rater_id}{rater's ID}
#'   \item{rater_sex}{rater's sex (female, male, intersex, NA)}
#'   \item{rater_age}{rater's age (17-90 years)}
#'   \item{rater_sexpref}{rater's preferred sex for romantic relationships (either, men, neither, women, NA)}
#'   \item{face_id}{face's ID}
#'   \item{face_sex}{face's sex (female, male)}
#'   \item{face_age}{face's age (in years)}
#'   \item{face_eth}{face's ethnic group}
#'   \item{rating}{attractiveness rating on a scale from 1 (much less attractive than average) to 7 (much more attractive than average)}
#' }
#' @source \doi{https://doi.org/10.6084/m9.figshare.5047666}
"faceratings"

#' Attractiveness rating subset
#'
#' The faceratings dataset cut down for demos to the first 4 raters of each sex 
#' and sexpref and the first 4 faces of each sex and ethnicity with non-NA ages
#'
#' @format A data frame with 768 rows and 9 variables:
#' \describe{
#'   \item{rater_id}{rater's ID}
#'   \item{rater_sex}{rater's sex (female, male)}
#'   \item{rater_age}{rater's age (17.4-54.3 years)}
#'   \item{rater_sexpref}{rater's preferred sex for romantic relationships (either, men, women)}
#'   \item{face_id}{face's ID}
#'   \item{face_sex}{face's sex (female, male)}
#'   \item{face_age}{face's age (19-47 years)}
#'   \item{face_eth}{face's ethnic group (black, east_asian, west_asian, white)}
#'   \item{rating}{attractiveness rating on a scale from 1 (much less attractive than average) to 7 (much more attractive than average)}
#' }
#' @source \doi{https://doi.org/10.6084/m9.figshare.5047666}
"fr4"