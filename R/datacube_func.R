#' Build data cube
#'
#' @param idu_geom IDU geometry saved as a sf object
#' @param delta_array Data frame containing Envision delta array
#' @param idu_field Name containing IDU index
#' @param fields Fields to include in the data cube
#' @param as_stars Return results as stars object
#'
#' @return datacube or stars object
#' @export
#'
#' @import dtplyr
#' @importFrom terra rast
#' @import dplyr
#' @import tidyr
#' @import stars
#' @import xml2
#' @import purrr
#' @import sf
#' @import abind
#' @importFrom data.table setDT

build_datacube <- function(
    delta_array,
    fields,
    idu_geom = NULL,
    run_name = NULL,
    idu_field = 'IDU_INDEX',
    as_stars = TRUE
  ){

    if(!exists('ref_data')){
      data('ref_data', package='envisionr')
      idu_geom <- ref_data$idu_vect
    }

    if(is.null(idu_geom)){
        stop('IDU reference required')
    }

    datacube <- purrr::map(fields, function(f){
        build_slice(delta_array, idu_geom, idu_field, f)
      }, .progress = TRUE) |>
      abind::abind(along=3)
    if(as_stars){
      datacube <- stars::st_as_stars(datacube) |>
        st_set_dimensions(1, names='idu') |> # set dim 1 to idu_index
        st_set_dimensions(2, names='year', values=c(2019:2059)) |> # set dim 2 to year
        split(3) # transfers fields to stars attributes
    }

    attr(datacube, 'run_name') <- run_name
    attr(datacube, 'date_created') <- Sys.Date()
    attr(datacube, 'filename') <- attr(delta_array, 'filename')

    return(datacube)
}

#' Build slice
#'
#' Builds component slice that are later combined into a single data cube
#'
#' @param delta_array
#' @param idu_geom
#' @param idu_field
#' @param fields
#' @param start_year
#' @param end_year
#'
#' @return data slice
#' @export

build_slice <- function(
    delta_array,
    idu_geom,
    idu_field = 'IDU_INDEX',
    fields,
    start_year=NULL,
    end_year=NULL
  ){

    if(is.null(start_year)){
      start_year <- min(delta_array$year)
    }

    if(is.null(end_year)){
      end_year <- max(delta_array$year)
    }

    setDT(delta_array)

    # Perform the operations
    delta_array <- delta_array[
      field %in% fields, .(idu, field, year, value = newValue)
    ][, year := factor(year, levels = start_year:end_year)
    ][, head(.SD, 1), by = c('idu','field','year')]

    # pull starting data from idu geometry
    if(any(names(idu_geom) == fields) == FALSE){
      idu_geom[,fields] <- NA
    }

    idu_dat <- pull_yr0_slice(idu_geom, fields, idu_field, start_year, end_year)

    delta_array <- bind_rows(idu_dat, delta_array) |>
      tidyr::complete(field, year, idu) |>
      dplyr::arrange(field, idu, year) |>
      dplyr::group_by(field, idu) |>
      tidyr::fill(value)

    dim1 <- unique(delta_array$idu)
    dim2 <- unique(delta_array$year)
    dim3 <- unique(delta_array$field)

    datacube <- array(
      data = delta_array |> dplyr::arrange(field, year, idu) |> dplyr::pull(value),
      dim = c(length(dim1), length(dim2), length(dim3)),
      dimnames = list(idu = dim1, year = dim2, field = dim3))

    return(datacube)
}

#' Pull yr0 slice
#'
#' Used to pull intial conditions from idu reference layer
#'
#' @param idu_geom
#' @param fields
#' @param idu_field
#' @param start_year
#' @param end_year
#'
#' @return IDU slice at year 0
#' @export

pull_yr0_slice <- function( # parameters
    idu_geom,
    fields,
    idu_field,
    start_year,
    end_year
  ){ # function
    idu_data <- idu_geom |>
      st_drop_geometry() |>
      dplyr::select(!!idu_field, !!fields) |>
      tidyr::pivot_longer(-idu_field, names_to = 'field') |>
      dplyr::mutate(year = 2019) |>
      dplyr::mutate(year = factor(year, start_year:end_year)) |>
      dplyr::rename(idu = !!idu_field)

    return(idu_data)
}
