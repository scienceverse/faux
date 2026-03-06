#' Make a correlation matrix
#'
#' `cormat` makes a correlation matrix from a single number, 
#' vars$\times$vars matrix, vars$\times$vars vector, or a vars$\times$(vars-1)/2 vector.
#'
#' @param cors the correlations among the variables (can be a single number, 
#' vars$\times$vars matrix, vars$\times$vars vector, or a vars$\times$(vars-1)/2 vector)
#' @param vars the number of variables in the matrix
#' 
#' @return matrix
#' @examples
#' cormat(.5, 3)
#' cormat(c( 1, .2, .3, .4,
#'          .2,  1, .5, .6, 
#'          .3, .5,  1, .7,
#'          .4, .6, .7,  1), 4)
#' cormat(c(.2, .3, .4, .5, .6, .7), 4)
#' @export
cormat <- function(cors = 0, vars = 3) {
  # correlation matrix
  if (is.numeric(cors) & length(cors) == 1) {
    if (cors >=-1 & cors <=1) {
      cors = rep(cors, vars*(vars-1)/2)
    } else {
      stop("cors must be between -1 and 1")
    }
  }
  
  if (vars == 1) {
    cor_mat <- matrix(1, nrow= 1)
  } else if (is.matrix(cors)) { 
    if (!is.numeric(cors)) {
      stop("cors matrix not numeric")
    } else if (dim(cors)[1] != vars || dim(cors)[2] != vars) {
      stop("cors matrix wrong dimensions")
    } else if (sum(cors == t(cors)) != (nrow(cors)^2)) {
      stop("cors matrix not symmetric")
    } else {
      cor_mat <- cors
    }
  } else if (length(cors) == vars*vars) {
    cor_mat <- matrix(cors, vars)
  } else if (length(cors) == vars*(vars-1)/2) {
    cor_mat <- cormat_from_triangle(cors)
  }
  
  # check matrix is positive definite
  if (!is_pos_def(cor_mat)) {
    stop("correlation matrix not positive definite")
  }
  
  return(cor_mat)
}

#' Make Correlation Matrix from Triangle
#'
#' `cormat_from_triangle` makes a correlation matrix from a vector of the upper right triangle
#'
#' @param cors the correlations among the variables as a vars\*(vars-1)/2 vector
#' 
#' @return matrix
#' @examples
#' cormat_from_triangle(c(.2, .3, .4, 
#'                            .5, .6, 
#'                                .7))
#' @export
cormat_from_triangle <- function(cors) {
  # get number of variables
  vars <- ceiling(sqrt(2*length(cors)))
  if (length(cors) != vars*(vars-1)/2) 
    stop("you don't have the right number of correlations")
  
  # generate full matrix from vector of upper right triangle
  cor_mat <- matrix(nrow=vars, ncol = vars)
  upcounter = 1
  lowcounter = 1
  for (col in 1:vars) {
    for (row in 1:vars) {
      if (row == col) {
        # diagonal
        cor_mat[row, col] = 1
      } else if (row > col) {
        # lower left triangle
        cor_mat[row, col] = cors[lowcounter]
        lowcounter <- lowcounter + 1
      }
    }
  }
  for (row in 1:vars) {
    for (col in 1:vars) {
      if (row < col) {
        # upper right triangle
        cor_mat[row, col] = cors[upcounter]
        upcounter <- upcounter + 1
      }
    }
  }
  
  cor_mat
}


#' Check a Matrix is Positive Definite
#'
#' `is_pos_def` makes a correlation matrix from a vector
#'
#' @param cor_mat a correlation matrix
#' @param tol the tolerance for comparing eigenvalues to 0
#' 
#' @return logical value 
#' @examples
#' is_pos_def(matrix(c(1, .5, .5, 1), 2)) # returns TRUE
#' is_pos_def(matrix(c(1, .9, .9, 
#'                    .9, 1, -.2, 
#'                    .9, -.2, 1), 3)) # returns FALSE
#' @export
is_pos_def <- function(cor_mat, tol=1e-08) {
  ev <- eigen(cor_mat, only.values = TRUE)$values
  sum(ev < tol) == 0
}

#' Limits on Missing Value for Positive Definite Matrix
#'
#' `pos_def_limits` returns min and max possible values for a positive definite matrix with a specified missing value
#'
#' @param ... the correlations among the variables as a vars\*(vars-1)/2 vector
#' @param steps the tolerance for min and max values
#' @param tol the tolerance for comparing eigenvalues to 0
#' 
#' @return dataframe with min and max values
#' @examples
#' pos_def_limits(.8, .2, NA)
#' @export
#' 
pos_def_limits <- function(..., steps = .01, tol = 1e-08) {
  cors <- unlist(list(...))
  if (sum(is.na(cors)) != 1) stop("cors needs to have exactly 1 NA")
  
  x <- seq(-1, 1, steps)
  l <- length(x)
  
  # search for min pos_def value
  ipd <- FALSE
  min <- 0
  while (!ipd & min < l) {
    min <- min + 1
    cors2 <- cors
    cors2[is.na(cors)] <- x[min]
    cft <- cormat_from_triangle(cors2)
    ipd <- is_pos_def(cft, tol = tol)
  }
  
  # search for max pos_def value only if ipd was ever true
  if (ipd) {
    ipd <- FALSE
    max <- l+1
    while (!ipd & max > 1) {
      max <- max - 1
      cors2 <- cors
      cors2[is.na(cors)] <- x[max]
      cft <- cormat_from_triangle(cors2)
      ipd <-  is_pos_def(cft, tol = tol)
    }
    
    min <- x[min]
    max <- x[max]
  } else {
    max <- NA
    min <- NA
  }
  
  list(
    "min" = min,
    "max" = max
  )
}
