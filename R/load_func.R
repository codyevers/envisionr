# Function to install, update, and load packages
#' Title
#'
#' @param ...
#'
#' @return
#' @export

load_packages <- function(...) {
  packages <- sapply(as.list(match.call())[-1], deparse)
  for (package in packages) {
    # Check if the package is already installed, install if necessary
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      install.packages(package)
    }
    # Load the package
    library(package, character.only = TRUE)
  }
}


#' Title
#'
#' @param folderPath
#'
#' @return
#' @export

load_datacubes <- function(folderPath) {

  # List all .RData files in the directory
  rdataFiles <- list.files(path = folderPath, pattern = "\\.rda$", full.names = TRUE)

  if(length(rdataFiles) == 0) {
    cat("No .RData files found in", folderPath, "\n")
    return(invisible(NULL)) # Return invisibly and do not proceed further
  }

  # Load each .RData file into the global environment
  for(file in rdataFiles) {
    load(file, envir = .GlobalEnv)
  }

  # Optional: Print a message after loading all files
  cat("All .RData files have been loaded into the global environment from", folderPath, "\n")
}


#' Title
#'
#' @param file
#' @param name
#'
#' @return
#' @export

read_datacube <- function(file, name = NULL) {
  if(tools::file_ext(file) != 'datacube'){
    stop('Must have datacube extension')
  }
  load(file)
  if (!exists("dc")) {
    stop("Object 'dc' was not found")
  }
  if(!is.null(name)){
    assign(name, value = dc, envir = .GlobalEnv)
    message(paste('Datacube loaded as', name))
  } else {
    return(dc)
  }
}


# Define the function
#' Title
#'
#' @param file_path
#' @param new_name
#'
#' @return
#' @export

load_and_rename_object <- function(file_path, new_name) {
  # Load the .RData file
  load(file_path)

  # Check if 'dc' is in the current environment
  if (!exists("dc")) {
    stop("The object 'dc' was not found in the loaded file.")
  }

  assign(new_name, value = dc, envir = .GlobalEnv)

  # Optionally, remove 'dc' from the environment if it's no longer needed
  remove(dc)

  # Return the new object
  return(get(new_name, envir = .GlobalEnv))
}
