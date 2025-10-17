# faux 1.2.3 (2025-09-25)

* Fixed failing tests due to ggplot2 4.0 ggplot object structure changes
* Moved to https://github.com/scienceverse/faux
* Website is now https://scienceverse.github.io/faux

# faux 1.2.2 (2025-01-17)

* Fixed some tests that were suddenly erroring on some platforms (hence the removal from CRAN)
* Removed some vignettes to make package size smaller for CRAN (they are still available on the website)

# faux 1.2.1.9002 (2024-02-10)

* Fixed a [bug](https://github.com/scienceverse/faux/issues/107) in `rmulti()` that assigned correlations to the wrong pairs with more than 3 variables (thanks @yann1cks!)
* Made `rmulti()` more efficient by skipping adjusted r simulation for normal-normal pairs
* Added `nbinom2norm()` conversion function, but not sure it works right unless you set `size` and `prob` manually (produces a warning if you don't)

# faux 1.2.1.9001 (2023-07-07)

* Fixed a bug in `interactive_design()` that didn't allow mu, sd or r with more than 0.1 accuracy, and gave incorrect error messages for r specifications with more than 1 value.

# faux 1.2.1 (2023-04-18)

* `add_between()` and `add_within()` don't convert non-character levels to factors any more * submitting to CRAN (sorry for letting it get archived!)

# faux 1.2.0 (2023-02-03)

* `rmulti()` function for multivariate distributions that aren't all normal (experimental)
* Minor changes to `add_random()`
* Fixed minor vignette bugs for CRAN
* Various minor bug fixes

# faux 1.1.0.9005 (2023-02-02)

* Fixed the `long` argument for `sim_df()`

# faux 1.1.0.9004 (2022-02-13)

* Improvements for `rmulti()` and associated helper functions `convert_r()` and `fh_bounds()`.
* A vignette for NORTA

# faux 1.1.0.9003 (2021-12-02)

* `rmulti()` function for multivariate distributions that aren't all normal (experimental)

# faux 1.1.0.9002 (2021-12-01)

* Likert distribution functions `rlikert()`, `dlikert()`, `plikert()` and `qlikert()`

# faux 1.1.0.9001 (2021-11-23)

* `add_random()` now names random factor items with the full random factor name (e.g., "class1", not "c1")
* `add_random()` allows you to set specific factor item names (see vignette)

# faux 1.1.0 (2021-09-13)

## Breaking changes

* `sim_design()` now names anonymous within and between factors like W and B or W1, W2, W3, ..., and B1, B2, ... instead of A, B, C, ... 

## New features

* `add_contrast()` and associated contr_code_*** functions
* `add_random()` and associated mixed design building functions

## Minor improvements and fixes

* `get_params()` doesn't need between, within, id, and dv set for date created by `sim_design()`
* `plot_design()` can display a subset of factors
* `sim_design()` fixed a bug in when setting n with an unnamed vector and within-subjects factors

# faux 1.0.0.9006 (2021-09-13)

* Added a vignette about creating random reports using the mixed effects building functions.

# faux 1.0.0.9006 (2021-08-30)

* fixed a bug in `sim_design()` when setting n with an unnamed vector and within-subjects factors (wouldn't run before).
* Updated `add_between()` and `add_within()` to make new columns factors with the same ordering as the specification
* Updated contrast functions to work with non-factor columns
* `add_between()` .prob argument works as expected now (and has tests)

# faux 1.0.0.9005 (2021-08-11)

* Updated contrasts vignette
* renamed `contr_code_deviation()` to `contr_code_anova()`

# faux 1.0.0.9004 (2021-08-10)

* Updated contrasts vignette
* new `add_contrast()` function
* renamed the contrast functions to all start with `contr_code_`

# faux 1.0.0.9003 (2021-08-09)

* added new contrast functions and changed labelling of others
* updated contrasts vignette 

# faux 1.0.0.9002 (2021-08-08)

* `plot_design()` can display a subset of factors
* updated plotting vignette to explain changing palettes
* updated mixed effects builder functions to avoid column name clashes
* added experimental contrast functions and vignette

# faux 1.0.0.9001 (2021-03-27)

* added new mixed effect builder functions
* updated mixed effects vignette
* anonymous within and between factors in `sim_design()` are now named W and B or W1, W2, W3, ..., B1, B2, ... instead of A, B, C, ... (and fixed relevant tests and vignette code)
* fixed `get_params()` so it doesn't need between, within, id, and dv set for date created by `sim_design()`

# faux 1.0.0 (2021-03-27)

* Released on CRAN
* Fixed a typo in faceratings for one face_eth ("eats asian/white" => "east asian/white")

# faux 0.0.1.65 (2021-03-23)

* Fixed a bug in `rnorm_pre()` when simulating a vector with correlations to more than 2 pre-existing vectors.
* Updated vignettes for new functions and changes.

# faux 0.0.1.64 (2021-03-19)

* `sim_design()` should no longer mangle level values in long format if they have underscores
* `sim_design()` should play better with different separator. FOr example, if you set `faux_options(sep = ".")` and have within-subject factors A and B with levels A_1/A_2 and B_1/B_2, your wide data will have columns A_1.B_1, A_1.B_2, A_2.B_1, A_2.B_2

# faux 0.0.1.63 (2021-03-14)

* fixed bug in `sim_design()` where parameters specified as a named vector couldn't be in a different order unless both between and within factors were specified (e.g., `mu = c(A2 = 2, A1 = 1)` resulted in a mu of 2 for A1 and 1 for A2).

# faux 0.0.1.62 (2021-03-13)

* new `sim_joint_dist()` function to simulate the joint distribution of categories
* `sim_df()` no longer breaks if there are NAs in the DV columns
* `sim_df()` now has an option to include missing data, it simulates the joint distribution of missingness for each between-subject cell
* some functions (`sim_df()` and `messy()`) can choose columns with unquoted names now (e.g., `messy(mtcars, .5, mpg)`)
* `messy()` now takes a vector of proportions so you can simulate different amounts of missing data per selected column
* `sample_from_pop()` is now vectorised

# faux 0.0.1.61 (2021-02-06)

* `get_params()` doesn't require within and between set for data made with faux (that has a "design" attribute)
* fixed bug in `get_params()` where the var column was alphabetised, but the corresponding columns for the correlation table were in factor order
* several new distribution conversion functions and a vignette
* this version isn't on CRAN yet

# faux 0.0.1.6 (2021-01-05)

* `nested_list()` updated to match scienceverse version and handle edge cases better
* changes to tests for changes in CRAN development environments
* `rnorm_multi()` can get column names from mu, sd, or r names

# faux 0.0.1.5 (2020-09-11)

* Removed a test using markdown that failed on Solaris (causing faux to be pulled from CRAN) Back on CRAN!
* `seed` arguments reinstated as deprecated and produce a warning

# faux 0.0.1.4 (2020-08-12)

* Even more fixes for CRAN (on CRAN from 2009-08-19!)
* Removed all `seed` arguments (at the request of CRAN)
* Faster unit tests

# faux 0.0.1.3 (2020-08-10)

* More fixes for CRAN (fingers crossed!)
* Added a `seed` argument to `rnorm_multi()`
* User options are no longer changed in vignettes

# faux 0.0.1.2 (2020-08-03)

* Removed some dependencies
* Various bug fixes
* Submitting to CRAN again!

# faux 0.0.1.1 (2020-06-25)

* Still not accepted to CRAN (but haven't worked on it in a long time)
* `nested_list` function for printing nested lists in Rmd
* New `codebook` function and vignette
* Interactive mode for codebooks (experimental)
* Updated for new tidyverse versions

# faux 0.0.1.0 (2020-02-27)

* Submitting to CRAN (wish me luck!)

# faux 0.0.0.9019 (2020-02-26)

* [Andrew Heiss](https://github.com/andrewheiss) added `norm2beta` function
* Added contributors (Andrew Heiss and Anna Krystali)
* `trunc2norm` now works if `min` or `max` are omitted.

# faux 0.0.0.9017 (2019-12-26)

* Added `rep` argument to `sim_design()` and `sim_data()`. If rep > 1, returns a nested data frame with `rep` simulated datasets.
* Fixed warnings on `get_params()`
* Improved `make_id()` function

# faux 0.0.0.9016 (2019-12-10)

* Removed ANOVApower

# faux 0.0.0.9015 (2019-07-22)

* Added distribution conversion functions (still experimental)

# faux 0.0.0.9014 (2019-06-09)

* More flexible parameter specification
* Set plotting default as a global option `faux_options(plot = TRUE)`
* Minor bug fixes and efficiency increases

# faux 0.0.0.9013 (2019-05-27)

* multi-factor cell names now in a more intuitive order (**will break old code that inputs parameters with unnamed vectors**)
* More flexible plotting styles (violin, jitter, box, pointrange with SD or SE, and combos)
* Minor bug fixes

# faux 0.0.0.9012 (2019-05-22)

* More flexible plots and data plots
* Setting seed doesn't change the seed in your global environment
* Plot labels for dv and id (set them like `dv = list(colname = "Name for Plots")`)

# faux 0.0.0.9011 (2019-05-19)

* Changed default DV column name from 'val' to 'y'
* Changed default ID column name from 'sub_id' to 'id'
* `sim_design()` can take intercept-only designs
* `rnorm_multi()` can take vars = 1 for intercept-only designs
* `json_design()` to output or save design specs in JSON format
* Added functions for converting between distributions

# faux 0.0.0.9010

* Fixed bugs in interactive specification
* Added `messy()` (thanks Emily)
* Better interactive specification (thanks Daniël)

# faux 0.0.0.9009

* Bug fix for `long2wide()` (handle designs with no between or no within factors)
* `sim_df()` returns subject IDs and takes data in long format
* Renamed `check_sim_stats()` to `get_params()`, which now returns the design
* Added interactive option to `sim_design()`

# faux 0.0.0.9008

* added `sim_mixed_cc()` to simulate null cross-classified mixed effect designs by subject, item and error SDs
* `sim_design()`, `sim_df()`, `sim_mixed_cc()` and `sim_mixed_df()` take a `seed` argument now for reproducible datasets

# faux 0.0.0.9007

* Added a plot option to `check_design()` and `sim_design()`
* Design lists returned by `check_design()` have a more consistent format 
    - n, mu, and sd are all data frames with between-cells as rows and within-cells as columns
    - `within` and `between` are named lists; factors and labels are no longer separately named

# faux 0.0.0.9006

* Changes to argument order and names (more consistent, but may break old scripts)
* Updated vignettes

# faux 0.0.0.9005

* Bug fixes for `sim_design()` (failed when within or between factor number was 0)

# faux 0.0.0.9004

* Added a `NEWS.md` file to track changes to the package.
* Added `sim_design()` to simulate data for mixed ANOVA designs.

