# Function to get run names from datacubes
get_run_names <- function(datacubes) {
  default_labels <- c("A", "B", "C", "D")  # Default identifiers
  run_names <- default_labels

  rename_default_labels = FALSE

  if(rename_default_labels){

    # Ensure datacubes is not empty
    if (length(datacubes) == 0 || all(sapply(datacubes, is.null))) {
      return(setNames(default_labels[1], "No Runs Loaded"))  # Return a default option
    }

    # Extract run names safely
    run_names <- sapply(seq_along(datacubes), function(i) {
      if (!is.null(datacubes[[i]]) && !is.null(attr(datacubes[[i]], 'run_name'))) {
        return(attr(datacubes[[i]], 'run_name'))
      }
      return(default_labels[i])  # Use default if run_name is missing
    })
  }

  # Construct named vector for selectInput
  run_choices <- setNames(default_labels[seq_along(datacubes)], run_names)

  return(run_choices)
}


#' Title
#'
#' @param filename
#'
#' @return
#' @export

read_delta_csv <- function(filename){
  da <- data.table::fread(filename)
  attr(da, 'filename') <- filename
  return(da)
}

#' Title
#'
#' @param file
#' @param name
#'
#' @return
#' @export

load_datacube <- function(file, name = NULL) {
  # if(tools::file_ext(file) != 'datacube'){
  #   stop('Must have datacube extension')
  # }
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

#' Title
#'
#' @param datacube
#' @param filename
#'
#' @return
#' @export

save_datacube <- function(datacube, filename){
  dc <- datacube
  save(dc, file = filename)
}
