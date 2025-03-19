#' Download delta array from OSU BEE website
#'
#' @param url_base
#' @param url_slug
#' @param directory
#' @param timeout
#'
#' @return Temporary folder where delta array is saved
#' @export
#'
download_delta_array <- function(
    url_slug = NULL,
    output_directory = NULL,
    url_base = "http://explorer.bee.oregonstate.edu/Topic/Deschutes/Outputs/Scenarios/",
    delete_temp_dir = FALSE,
    timeout = 1200){

  if(is.null(url_slug)) stop('URL slug required')
  if(is.null(output_directory)) stop('Output directory required')

  # url for downloading delta array
  url = file.path(url_base, url_slug)

  # set timeout (for large downloads)
  options(timeout = timeout)

  # make temporary directory
  temp_dir <- tempdir()
  message(glue::glue('Temp download directory:\n {temp_dir}'))

  # download delta array zip to temporary directory (3GB file at 4MB/sec = 12 minutes)
  download.file(url, destfile = file.path(temp_dir, 'da.zip'))

  # unzip the file (30 seconds)
  unzip(file.path(temp_dir, 'da.zip'), exdir = temp_dir)

  # identify delta array csv
  delta_array_csv_fn <- file.path(temp_dir, list.files(path = temp_dir, recursive = T, pattern = '.csv'))

  if(!is.null(output_directory)){
    file.copy(delta_array_csv_fn, output_directory, overwrite = T)
    message(glue::glue('Delta array CSV copied to:\n{output_directory}'))

    if(delete_temp_dir){
      unlink(file.path(temp_dir, "*"), recursive = TRUE)
    }
  }
}



#' Download and build data cube from delta array at given URL
#'
#' @param url
#'
#' @return
#' @export

download_and_build_data_cube <- function(
    url,
    idu_shp = NULL,
    fields = NULL
    ){

  if(!exists("idu_shp")){
    stop('"idu_shp" is required')
  }

  # Destination folder for the unzipped files
  dest_folder <- "data/csv"

  # Temporary file to store downloaded zip
  temp_dir <- tempdir()
  print(temp_dir)

  # Download the file (4 minutes)
  download.file(url, destfile = file.path(temp_dir, 'da.zip'), timeout = 1000)

  # Unzip the file (30 seconds
  unzip(file.path(temp_dir, 'da.zip'), exdir = temp_dir)

  csv <- file.path(
    temp_dir,
    list.files(path = temp_dir, recursive = T, pattern = '.csv')
  )

  # read csv (1 minute)
  delta_array <- data.table::fread(csv)

  # build datacube (1 minute)
  dc <- build_datacube(
    idu_geom = idu_shp,
    delta_array = delta_array,
    idu_field='IDU_INDEX',
    fields = fields
  )

  # Get a list of all files in the temp directory
  files_to_delete <- list.files(temp_dir, full.names = TRUE, recursive = T)

  # Delete all files in the temp directory
  file.remove(files_to_delete)

  return(dc)
}



