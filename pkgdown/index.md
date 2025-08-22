# faux <img src="reference/figures/logo.png" align="right" alt="" width="120" />
<!-- rmarkdown v1 -->

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/163506566.svg)](https://zenodo.org/badge/latestdoi/163506566)
![CRAN version](https://www.r-pkg.org/badges/version-last-release/faux)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/faux)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Travis build status](https://travis-ci.org/scienceverse/faux.svg?branch=master)](https://travis-ci.org/scienceverse/faux)
[![Coverage status](https://codecov.io/gh/scienceverse/faux/branch/master/graph/badge.svg)](https://codecov.io/github/scienceverse/faux?branch=master)
[![R-CMD-check](https://github.com/scienceverse/faux/workflows/R-CMD-check/badge.svg)](https://github.com/scienceverse/faux/actions)
<!-- badges: end -->





It is useful to be able to simulate data with a specified structure. The `faux` package provides some functions to make this process easier. See the [vignettes](articles/) for more details.

## Installation

You can install the released version of faux from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("faux")
```

And the development version from [GitHub](https://github.com/scienceverse/faux) with:

``` r
# install.packages("devtools")
devtools::install_github("scienceverse/faux")
```

## Quick overview

### Simulate data for a factorial design

See the [Simulate by Design vignette](articles/sim_design.html) for more details.


``` r
between <- list(pet = c(cat = "Cat Owners", 
                        dog = "Dog Owners"))
within <- list(time = c("morning", 
                        "noon", 
                        "evening", 
                        "night"))
mu <- data.frame(
  cat    = c(10, 12, 14, 16),
  dog    = c(10, 15, 20, 25),
  row.names = within$time
)
df <- sim_design(within, between, 
                 n = 100, mu = mu, 
                 sd = 5, r = .5)
```

![Default design plot](man/figures/plot-sim-design-1.png)



``` r
p1 <- plot_design(df)
p2 <- plot_design(df, "pet", "time")

cowplot::plot_grid(p1, p2, nrow = 2, align = "v")
```

![Plot the data with different visualisations.](man/figures/plot-design-1.png)

### Simulate new data from an existing data table

See the [Simulate from Existing Data vignette](articles/sim_df.html) for more details.


``` r
new_iris <- sim_df(iris, 50, between = "Species") 
```

![Simulated iris dataset](man/figures/plot-iris-sim-1.png)

### Simulate data for a mixed design

You can build up a cross-classified or nested mixed effects design using piped functions. See the [contrasts vignette](articles/contrasts.html) for more details.


``` r
# simulate 20 classes with 20 to 30 students per class
data <- add_random(class = 20) %>%
  add_random(student = sample(20:30, 20, replace = TRUE), 
             .nested_in = "class") %>%
  add_between(.by = "class", 
              school_type = c("private","public"), 
              .prob = c(5, 15)) %>%
  add_between(.by = "student",
              gender = c("M", "F", "NB"),
              .prob = c(.49, .49, .02))
```



|school_type |gender |   n|
|:-----------|:------|---:|
|private     |M      |  73|
|private     |F      |  55|
|private     |NB     |   1|
|public      |M      | 168|
|public      |F      | 201|
|public      |NB     |   8|





## Other simulation packages

I started this project as a collection of functions I was writing to help with my own work. It's one of many, many simulation packages in R; here are some others. I haven't used most of them, so I can't vouch for them, but if faux doesn't meet your needs, one of these might.

* [SimDesign](https://cran.r-project.org/web/packages/SimDesign/vignettes/SimDesign-intro.html): generate, analyse and summarise data from models or probability density functions
* [simstudy](https://kgoldfeld.github.io/simstudy/): Simulation of Study Data
* [simr](https://github.com/pitakakariki/simr): Power Analysis of Generalised Linear Mixed Models by Simulation
* [simulator](http://github.com/jacobbien/simulator): streamlines the process of performing simulations by creating a common infrastructure that can be easily used and reused across projects
* [lsasim](https://github.com/tmatta/lsasim): Simulate large scale assessment data 
* [simmer](https://r-simmer.org/): Trajectory-based Discrete-Event Simulation (DES
* [parSim](https://cran.r-project.org/web/packages/parSim/): Parallel Simulation Studies

