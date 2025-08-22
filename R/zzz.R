## set default options for faux_options:
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.faux <- list(
    faux.connection = stdin(),
    faux.sep = "_",
    faux.plot = TRUE,
    faux.verbose = TRUE,
    faux.long = FALSE
  )
  toset <- !(names(op.faux) %in% names(op))
  if(any(toset)) options(op.faux[toset])
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  paste(
    "\n************",
    "Welcome to faux. For support and examples visit:",
    "https://scienceverse.github.io/faux/",
    "- Get and set global package options with: faux_options()",
    "************",
    sep = "\n"
  ) %>% packageStartupMessage()
}
