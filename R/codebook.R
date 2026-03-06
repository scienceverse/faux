#' Create PsychDS Codebook from Data
#' 
#' See \code{vignette("codebook", package = "faux")} for details.
#'
#' @param data The data frame to generate a codebook for
#' @param name The name of this dataset (if NULL, will be the same as `data`, limited to 64 characters)
#' @param vardesc Optional variable properties in the format of a named list of vectors (can be named or unnamed and in the same order as the data) from the options "description", "privacy", "dataType", "identifier", "minValue", "maxValue", "levels", "levelsOrdered", "na", "naValue", "alternateName", "privacy", "unitCode", "unitText"
#' @param ... Further dataset properties (e.g., description, license, author, citation, funder, url, identifier, keywords, privacyPolicy)
#' @param schemaVersion defaults to "Psych-DS 0.1.0"
#' @param return Whether the output should be in JSON format (json), a list (list) or the reformatted data with the codebook as an attribute (data)
#' @param interactive Whether the function should prompt the user to describe columns and factor levels
#'
#' @return a list or json-formatted codebook, or reformatted data with the codebook as an attribute
#' @export
#'
#' @examples
#'
#' vardesc = list(
#'   description = c("Length of the sepal",
#'                   "Width of the sepal",
#'                   "Length of the petal",
#'                   "Width of the petal",
#'                   "The flower species"),
#'   type = c("float", "float", "float", "float", "string")
#' )
#' codebook(iris, vardesc = vardesc)
#'
codebook <- function(data, name = NULL, vardesc = list(), ...,
                     schemaVersion = "Psych-DS 0.1.0",
                     return = c("json", "list", "data"),
                     interactive = FALSE
) {
  # use PsychDS format from https://docs.google.com/document/d/1u8o5jnWk0Iqp_J06PTu5NjBfVsdoPbBhstht6W0fFp0/edit
  
  if (is.null(name)) {
    name <- utils::capture.output(match.call()$data)
    name <- name[[1]] 
    name <- substr(name, 1, 64) # in case name is a long function call
    if (name == ".") name <- "[unnamed data]" # df passed through pipe
  }
  
  # all possible type abbreviations ----
  types <- c(i = "int",
             int = "int",
             integer = "int",
             
             s = "string",
             string = "string",
             factor = "string",
             char = "string",
             character = "string",
             
             f = "float",
             float = "float",
             double = "float",
             numeric = "float",
             
             b = "bool",
             bool = "bool",
             boolean = "bool",
             logical = "bool")
  
  schema <- list(
    "@context" = "https://schema.org/",
    "@type" = "Dataset",
    name = name,
    schemaVersion = schemaVersion
  )
  
  # check datadesc
  datadesc <- list(...)
  
  # fix doi and relabel as sameAs ----
  if (!is.null(datadesc$doi)) {
    doi <- tolower(datadesc$doi) %>%
      gsub("\\s", "", .) %>%
      sub("^doi\\:", "", .) %>%
      sub("^https://doi.org/", "", .)
    datadesc$doi <- NULL
    datadesc$identifier <- paste0("https://doi.org/", doi)
  }
  
  possible_vals <- c("license", "author", "citation", "funder", "url", "identifier", "privacyPolicy", "keywords")
  non_standard <- setdiff(names(datadesc), possible_vals)
  if (length(non_standard) > 0) {
    warning("The following dataset properties are not standard: ",
            paste(non_standard, collapse = ", "), "\n")
  }
  
  # add data properties ----
  schema <- c(schema, datadesc)
  
  # get vardesc from design
  design <- get_design(data)
  if (!is.null(design)) {
    win <- ifelse(length(design$within) > 0,
                  names(design$within), "")
    if (win %in% names(data)) {
      # in long format
      lvls <-  c(design$between, design$within)
      descs <- c(design$id, design$dv)
    } else {
      lvls <- design$between
      if (length(design$within) > 0) {
        # add within levels as vardesc
        cnames <- cell_combos(design$within)
        exp <- expand.grid(rev(design$within))
        cells <- apply(exp, 1, function(x) { 
          paste(rev(x), collapse = " ") 
        })
        names(cells) <- cnames
        descs <- c(design$id, design$dv, cells)
      } else {
        descs <- c(design$id, design$dv)
      }
    }
    
    # overwrite levels and descriptions in design with vardesc
    if (length(lvls) > 0) {
      # only run this if there are any factor columns
      for (n in names(vardesc$levels)) {
        lvls[[n]] <- as.list(vardesc$levels[[n]])
      }
      vardesc$levels <- lvls
    }
    for (n in names(vardesc$description)) {
      descs[[n]] <- vardesc$description[[n]]
    }
    vardesc$description <- descs
  }
  
  # check vardesc ----
  possible_vals <- c("name", "description", "privacy",
                     "dataType", "identifier", "minValue", "maxValue",
                     "levels", "levelsOrdered", "na", "naValue",
                     "alternateName", "privacy", "unitCode", "unitText")
  
  non_standard <- setdiff(names(vardesc), possible_vals)
  if (length(non_standard) > 0) {
    warning("The following variable properties are not standard: ",
            paste(non_standard, collapse = ", "), "\n")
  }
  
  # TODO: add more validation ----
  
  # make variableMeasured ----
  if (ncol(data) > 0) {
    vm <- list()
    colnames <- names(data)
    
    for (i in 1:ncol(data)) {
      col <- colnames[i]
      
      # @type and name ----
      vm[[i]] <- list(
        `@type` = "PropertyValue",
        name = col,
        description = col # default to be replaced from vardesc
      )
      
      # set variable attributes from vardesc ----
      for (vd in names(vardesc)) {
        vals <- vardesc[[vd]]
        
        if (!is.null(names(vals))) {
          # set from named (if available)
          if (col %in% names(vals)) vm[[i]][vd] <- vals[col]
        } else if (length(vals) == ncol(data)) {
          # set from position
          vm[[i]][vd] <- vals[i]
        } else if (length(vals) == 1) {
          # set all to same
          vm[[i]][vd] <- vals[1]
        } else {
          warning("Couldn't set ", vd, " for ", col)
        }
      }
      
      # set type if not specified ---
      if (!is.null(vm[[i]]$dataType)) {
        if ((vm[[i]]$dataType %in% names(types))) {
          vm[[i]]$dataType <- types[[vm[[i]]$dataType]]
        } else {
          # not a valid type, so set to null and get from data
          warning(vm[[i]]$name, " does not have a valid dataType (",
                  vm[[i]]$dataType, ")")
          vm[[i]]$dataType <- NULL
        }
      }
      
      if (is.null(vm[[i]]$dataType)) {
        can_be_int <- isTRUE(all(
          data[[i]] == suppressWarnings(as.integer(data[[i]]))
        ))
        
        vm[[i]]$dataType <- dplyr::case_when(
          is.factor(data[[i]]) ~ "string",
          is.character(data[[i]]) ~ "string",
          is.integer(data[[i]]) ~ "int",
          is.numeric(data[[i]]) & can_be_int ~ "int",
          is.numeric(data[[i]]) ~ "float",
          is.logical(data[[i]]) ~ "bool",
          TRUE ~ typeof(data[[i]])
        )
        if (faux_options("verbose")) {
          message(vm[[i]]$name, " set to dataType ", vm[[i]]$dataType)
        }
      }
      
      # get levels for factors if not specified ----
      if (is.factor(data[[i]]) | !is.null(vm[[i]]$levels)) {
        if (is.null(vm[[i]]$levels)) {
          lvls <- levels(data[[i]])
          names(lvls) <- lvls
          vm[[i]]$levels <- lvls
        }
        # make sure it is a list because named vectors don't render right in jsonlite
        vm[[i]]$levels <- as.list(vm[[i]]$levels)
        
        if (is.null(vm[[i]]$levelsOrdered)) {
          vm[[i]]$levelsOrdered <- is.ordered(data[[i]])
        }
      }
    }
    schema$variableMeasured <- vm
  }
  
  if (isTRUE(interactive)) {
    schema <- codebook_interactive(data, schema)
  }
  
  # return correct format ----
  return <- match.arg(return)
  
  if (return == "json") {
    schema <- schema %>%
      jsonlite::toJSON(auto_unbox = TRUE) %>%
      jsonlite::prettify(4)
  } else {
    class(schema) <- c("psychds_codebook", "list")
  }
  
  if (return == "data") {
    # convert data ----
    r_pds <- c(character = "string",
               logical = "bool",
               integer = "int",
               double = "float")
    for (v in schema$variableMeasured) {
      col <- data[[v$name]]
      ctype <- typeof(col)
      if (r_pds[ctype] != v$dataType) {
        message("Converting ", v$name, " from ", r_pds[ctype], " to ", v$dataType)
        # types don't match so convert
        suppressWarnings(
          if (v$dataType == "string") {
            convcol <- as.character(col)
          } else if (v$dataType == "bool") {
            convcol <- as.logical(col)
          } else if (v$dataType == "int") {
            convcol <- as.integer(col)
          } else if (v$dataType == "float") {
            convcol <- as.double(col)
          } else {
            convcol <- col
          }
        )
        
        # check conversions are equivalent
        if (isTRUE(all(convcol == col))) {
          data[v$name] <- convcol
        } else {
          message("- Error, not converted")
        }
      }
    }
    
    attr(data, "codebook") <- schema
    return(data)
  } else {
    return(schema)
  }
}


#' Print Codebook Object
#'
#' @param x The psychds_codebook list
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#' @returns Prints x and returns it invisibly
#'
print.psychds_codebook <- function(x, ...) {
  
  if ("Psych-DS 0.1.0" == x$schemaVersion &
      length(x$variableMeasured) > 0) {
    
    txt <- ""
    txt <- sprintf("%sCodebook for %s (Psych-DS 0.1.0)\n\n",
                   txt, x$name)
    
    # list dataset parameters ----
    txt <- sprintf("%sDataset Parameters\n\n", txt)
    omit_names <- c("@context", "@type", "variableMeasured")
    dataset_params <- setdiff(names(x), omit_names)
    txt <- sprintf("%s%s", txt, nested_list(x[dataset_params]))
    
    # list column parameters ----
    txt <- sprintf("%s\n\nColumn Parameters\n\n", txt)
    vars <- list()
    for (v in x$variableMeasured) {
      desc <- ifelse(v$name == v$description,
                     "", paste(":", v$description))
      extras <- ""
      
      # has levels ----
      if (length(v$levels) > 0) {
        if (is.null(names(v$levels))) {
          lvls <- v$levels
        } else if (all(names(v$levels) == v$levels)) {
          lvls <- v$levels
        } else {
          lvls <- paste0(names(v$levels), ": ", v$levels)
        }
        
        extras <- sprintf(
          "\n  * Levels\n    * %s\n  * Ordered: %s",
          paste(lvls, collapse = "\n    * "),
          ifelse(is.null(v$levelsOrdered), FALSE, v$levelsOrdered)
        )
      }
      
      vars[v$name] = sprintf(
        "* %s (%s)%s%s",
        v$name, v$dataType, desc, extras
      )
    }
    
    txt <- sprintf("%s%s", txt, paste(vars, collapse = "\n"))
    
    cat(txt)
  } else {
    utils::str(x)
  }
}



