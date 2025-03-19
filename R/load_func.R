# Function to get run names from datacubes
get_run_names <- function(datacubes) {

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

  return(run_names)
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

load_datacube <- function(file, name = NULL, parse_vegclass = FALSE) {
  # if(tools::file_ext(file) != 'datacube'){
  #   stop('Must have datacube extension')
  # }
  load(file)
  if (!exists("dc")) {
    stop("Object 'dc' was not found")
  }

  if(parse_vegclass){
    message('Parsing VEGCLASS into CoverType, SIZE, CANOPY, LAYERS')
    dc <- dc |>
      mutate(CoverType = stringr::str_sub(VEGCLASS, 1, 3) |> as.numeric()) |>
      mutate(SIZE = stringr::str_sub(VEGCLASS, 4, 4) |> as.numeric()) |>
      mutate(CANOPY = stringr::str_sub(VEGCLASS, 5, 5) |> as.numeric()) |>
      mutate(LAYERS = stringr::str_sub(VEGCLASS, 6, 6) |> as.numeric())
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
