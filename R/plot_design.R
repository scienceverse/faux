#' Plot design
#'
#' Plots the specified within and between design. See \code{vignette("plots", package = "faux")} for examples and details.
#'
#' @param x A list of design parameters created by check_design() or a data tbl (in long format)
#' @param ... A list of factor names to determine visualisation (see vignette) in the order color, x, facet row(s), facet col(s)
#' @param geoms A list of ggplot2 geoms to display, defaults to "pointrangeSD" (mean ± 1SD) for designs and c("violin", "box") for data, options are: pointrangeSD, pointrangeSE, violin, box, jitter
#' @param palette A brewer palette, defaults to "Dark2" (see ggplot2::scale_colour_brewer)
#' @param labeller How to label the facets (see ggplot2::facet_grid). "label_value" is used by default.
#' 
#' @return plot
#' 
#' @examples 
#' 
#' within <- list(time = c("day", "night"))
#' between <- list(pet = c("dog", "cat"))
#' des <- check_design(within, between, plot = FALSE)
#' plot_design(des)
#' 
#' data <- sim_design(within, between, plot = FALSE)
#' plot_design(data)
#' 
#' @export
#' 
plot_design <- function(x, ..., geoms = NULL, palette = "Dark2", labeller = "label_value") {
  outlier.alpha <- 1 # default value, might be overridden
  
  # turn x to data and design ----
  if (!is.data.frame(x) && is.list(x)) {
    if (is.null(geoms)) geoms <- "pointrangeSD"
    design <- x
    if ("pointrangeSE" %in% geoms) {
      # don't change Ns
    } else if ("violin" %in% geoms | "box" %in% geoms) {
      # large N for smooth violins and boxes
      design$n <- lapply(design$n, function(x){10000})
      outlier.alpha <- 0
    } else {
      # smallish N for speed
      design$n <- lapply(design$n, function(x){100})
    }
    
    data <- sim_data(design = design, empirical = TRUE, long = TRUE)
  } else if (is.data.frame(x)) {
    if (is.null(geoms)) geoms <- c("violin", "box")
    data <- x
    design <- get_design(data)
    if (is.null(design)) {
      stop("The data table must have a design attribute")
    }
    if (all(names(data)[1:2] == c("rep", "data"))) {
      # nested data, just graph first row
      data <- data$data[[1]] %>%
        set_design(design)
    }
    if (!(names(design$dv) %in% names(data))) {
      # get data into long format
      data <- wide2long(data)
    }
  } else {
    stop("x must be a design list or a data frame")
  }
  
  # set factors to plot ----
  factors <- c(design$within, design$between)
  # if ("rep" %in% names(data)) {
  #   factors$rep <- as.list(unique(data$rep))
  #   names(factors$rep) <- factors$rep
  # }
  f <- syms(names(factors)) # make it possible to use strings to specify columns
  dv <- sym(names(design$dv))
  
  if (c(...) %>% length()) {
    f <- syms(c(...))
  }
  factor_n <- length(f)
  
  # use long names for factors ----
  for (col in f) {
    lvl <- factors[[col]] %>% names()
    lbl <- factors[[col]]
    data[[col]] <- factor(data[[col]], levels = lvl, labels = lbl)
  }
  
  if (factor_n == 0) {
    p <- ggplot(data, aes(x = 0, y = !!dv, fill = "red", color = "red")) +
      xlab(design$dv[[1]]) + theme_bw() +
      theme(axis.text.x.bottom = element_blank(),
            axis.ticks.x.bottom = element_blank(),
            legend.position = "none")
  } else if (factor_n == 1) {
    p <- ggplot(data, aes(!!f[[1]], !!dv,
                          fill = !!f[[1]],
                          color = !!f[[1]])) + 
      theme_bw() +
      theme(legend.position = "none") + 
      labs(x = design$vardesc[[rlang::as_string(f[[1]])]])
  } else {
    p <- ggplot(data, aes(!!f[[2]], !!dv,
                          fill = !!f[[1]],
                          color = !!f[[1]])) + 
      theme_bw() + 
      labs(x = design$vardesc[[rlang::as_string(f[[2]])]],
           fill = design$vardesc[[rlang::as_string(f[[1]])]],
           color = design$vardesc[[rlang::as_string(f[[1]])]])
  }
  
  # create labelling function ----
  if ((is.function(labeller) && 
      isTRUE(all.equal.function(labeller, label_both))) ||
      (is.character(labeller) && labeller == "label_both")) {
    label_func <- f[-(1:2)] %>%
      lapply(rlang::as_string) %>%
      stats::setNames(., .) %>%
      lapply(function(nm) {
        label <- design$vardesc[[nm]]
        names <- unlist(factors[[nm]])
        trans_list <- stats::setNames(paste0(label, ": ", names), names)
        ggplot2::as_labeller(trans_list)
      }) %>%
      do.call(ggplot2::labeller, .)
  } else {
    label_func <- labeller
  }
  
  if (factor_n > 2) {
    expr <- switch(factor_n,
      NULL,
      NULL,
      dplyr::expr(!!f[[3]] ~ .),
      dplyr::expr(!!f[[3]] ~ !!f[[4]]),
      dplyr::expr(!!f[[3]] ~ !!f[[4]] * !!f[[5]]),
      dplyr::expr(!!f[[3]] * !!f[[4]] ~ !!f[[5]] * !!f[[6]])
    )
    p <- p + facet_grid(eval(expr), labeller = label_func)
  }
  
  # add text y-label to all plots
  p <- p + ylab(design$dv[[1]])
  
  if ("jitter" %in% geoms) {
    p <- p + geom_point(position = position_jitterdodge(
      jitter.width = .5, jitter.height = 0, dodge.width = 0.9
    ))
  } 
  if ("violin" %in% geoms) {
    p <- p + geom_violin(color = "black", alpha = 0.5,
                         position = position_dodge(width = 0.9))
  } 
  if ("box" %in% geoms) {
    p <- p + geom_boxplot(width = 0.25, color = "black",
                   position = position_dodge(width = 0.9),
                   show.legend = FALSE, outlier.alpha = outlier.alpha)
  }
  if ("pointrangeSD" %in% geoms | "pointrangeSE" %in% geoms) {
    if ("pointrangeSD" %in% geoms) {
      minsd <- function(x) { mean(x) - sd(x) }
      maxsd <- function(x) { mean(x) + sd(x) }
      shape <- 10
      size <- .5
    } else if ("pointrangeSE" %in% geoms) {
      minsd <- function(x) { mean(x) - sd(x)/sqrt(length(x)) }
      maxsd <- function(x) { mean(x) + sd(x)/sqrt(length(x)) }
      shape <- 20
      size <- .5
    }
    
    p <- p + stat_summary(
      fun = mean, 
      fun.min = minsd,
      fun.max = maxsd,
      geom='pointrange', 
      shape = shape,
      #size = size,
      position = position_dodge(width = 0.9))
  }
  
  p + scale_colour_brewer(palette = palette) + 
      scale_fill_brewer(palette = palette)
}


#' Plot from faux design
#'
#' @method plot design
#' @export
#' @describeIn plot_design Plotting from a faux design list
plot.design <- function(x, ...) {
  plot_design(x, ...)
}

#' Plot from faux data
#'
#' @method plot faux
#' @export
#' @describeIn plot_design Plotting from a faux data table
plot.faux <- function(x, ...) {
  plot_design(x, ...)
}


