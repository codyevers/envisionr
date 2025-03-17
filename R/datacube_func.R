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
#' @import dplyr
#' @import dtplyr
#' @import tidyr
#' @import stars
#' @import xml2
#' @import purrr
#' @import sf
#' @import abind
#' @importFrom terra rast
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
    } else {
      idu_geom <- ref_data$idu_vect
    }

    if(is.null(idu_geom)){
        stop('IDU reference required')
    }

    yr_range <- range(delta_array$year)

    slices <- fields |> purrr::map(function(field){
        build_slice(
          delta_array = delta_array,
          idu_geom = idu_geom,
          idu_field = idu_field,
          fields = field,
          start_year = yr_range[1],
          end_year = yr_range[2])
      }, .progress = TRUE)

    # bind slices into a cubes
    datacube <- slices |> abind::abind(along=3)

    if(as_stars){
      datacube <- stars::st_as_stars(datacube) |>
        st_set_dimensions(1, names='idu') |> # set dim 1 to the idu index
        st_set_dimensions(2, names='year') |> # set dim 2 to year
        split(3) # transfers fields to stars attributes
    }

    attr(datacube, 'run_name') <- run_name
    attr(datacube, 'date_created') <- Sys.Date()

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

    # filter to field f and select/rename columns; convert year to a factor,
    # select first row in case of duplicates
    delta_array <- delta_array[
      field %in% fields, .(idu, field, year, value = newValue)
    ][, year := factor(year, levels = start_year:end_year)
    ][, head(.SD, 1), by = c('idu','field','year')]

    if(any(names(idu_geom) == fields) == FALSE){
      idu_geom[,fields] <- NA
    }

    # pull starting data from idu geometry
    year0_values <- pull_yr0_slice(
      idu_geom = idu_geom,
      fields = fields,
      idu_field = idu_field,
      start_year = start_year,
      end_year = end_year)

    # combine year0 w/ delta array, fill in missing combos, and fill w/ value
    delta_array_c <- bind_rows(year0_values, delta_array) |>
      tidyr::complete(field, year, idu) |>
      dplyr::arrange(field, idu, year)

    # fill missing values by replacing missing data from top to bottom
    delta_array_a_f <- delta_array_c |>
      dplyr::group_by(field, idu) |>
      tidyr::fill(value)

    # pull values in reverse order of review order of dimensions
    vals <- delta_array_a_f |>
      dplyr::arrange(field, year, idu) |>
      dplyr::pull(value)

    # define dimensions values
    idu_dim <- unique(delta_array_a_f$idu)
    year_dim <- unique(delta_array_a_f$year)
    field_dim <- unique(delta_array_a_f$field)

    # arrange values in slice
    slice <- array(
      data = vals,
      dim = c(length(idu_dim), length(year_dim), length(field_dim)),
      dimnames = list(idu = idu_dim, year = year_dim, field = field_dim))

    return(slice)
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
      sf::st_drop_geometry() |>
      dplyr::select(!!idu_field, !!fields) |>
      tidyr::pivot_longer(-idu_field, names_to = 'field') |>
      dplyr::mutate(year = start_year - 1) |>
      dplyr::mutate(year = factor(year, c(start_year-1,start_year:end_year))) |>
      dplyr::rename(idu = !!idu_field)

    return(idu_data)
}
