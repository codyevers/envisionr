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
