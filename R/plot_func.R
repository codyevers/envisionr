#' Update IDU raster
#'
#' @param raster
#' @param datacube
#' @param field
#' @param year
#' @param query
#'
#' @return
#' @export

update_raster <- function(
  raster,
  datacube,
  field = 1,
  year = 1,
  extent = NULL,
  queryA = '',
  queryB = ''
){

  index <- c(raster$IDU_INDEX) + 1

  if(class(datacube) == 'stars'){
    # query is blank
    if(queryA == '' & queryB == ''){
      raster$VALUE  <- datacube[field, index, year] |> dplyr::pull(!!field) |> c()
    } else {
    # query is applied
      x <- apply_query(datacube, queryA, queryB, year)
      raster$VALUE <- x[index]
    }
  } else if (class(datacube) == 'array'){
    raster$VALUE <- datacube[index, year, field]
  } else {
    stop('datacube must be of class stars or array')
  }

  # convert to terra rast:object
  ras <- terra::rast(raster[c('VALUE'),])

  # look for extent
  if(!is.null(extent)){
    try({
      ras <- ras |> terra::crop(extent)
    })
  }

  return(ras)
}


#' Apply query to data cube
#'
#' @param queryA
#' @param queryB
#' @param datacube
#' @param year
#'
#' @return
#' @export

apply_query <- function(
    datacube,
    queryA,
    queryB,
    year = NULL
){

  eval_txt <- paste0('with(datacube, ', queryA, ')')
  out_2d <- parse(text = eval_txt) |> eval()

  if(queryB != ''){
    eval_txt <- paste0('with(datacube, ', queryB, ')')
    out_2d_b <- parse(text = eval_txt) |> eval()
    out_2d_b = out_2d_b * 10
  }

  if(queryA != '' & queryB != ''){
    out_2d <- out_2d + out_2d_b
  }

  # browser()
  if (is.null(year)) { # sum across all years if year is NULl
    out_1d <- apply(out_2d, 1, sum)
  } else {
    out_1d <- out_2d[,year]
  }
  return(out_1d)
}


#' Plot raster
#'
#' @param raster
#' @param pal_lookup
#' @param extent
#' @param options character vector of map elements
#' @param maxcell
#'
#' @details
#' options include Roads, Zones, and Legend
#'
#' @return
#' @export

plot_raster <- function(
  raster,
  pal_lookup = NULL,
  pal_table = NULL,
  extent = NULL,
  focus = NULL,
  options = c('Roads','Zones','Legend'),
  maxcell=1e5
){ # function

  ras <- raster

  if(!is.null(pal_lookup)){
    pal_df <- get_pal(pal_lookup)
  }

  if(!is.null(pal_table)){
    pal_df <- pal_table
  }

  if(is.null(pal_lookup)){
    # no palette info
    terra::plot(ras,
         maxcell = maxcell,
         axes = any(grepl('Axes', options)),
         legend = any(grepl('Legend', options)))
    } else {
      if(any(names(pal_df) == 'minVal')){
        # continuous variable
        terra::plot(ras,
             col = pal_df$hex,
             breaks = c(pal_df$minVal, Inf),
             maxcell = maxcell,
             axes = any(grepl('Axes', options)),
             legend = any(grepl('Legend', options)))
      } else {
        # categorical variable
        ras <- terra::as.factor(ras)
        pal_df <- pal_df |>
          dplyr::select(value, col = hex) |>
          dplyr::mutate(value = as.numeric(value)) |>
          as.data.frame()
        terra::coltab(ras) <- pal_df
        terra::plot(ras,
             maxcell = maxcell,
             axes = any(grepl('Axes', options)),
             legend = any(grepl('Legend', options)))
      }
  }

  # plot accessory elements
  if(!is.null(focus)){
    plot(focus, pch=4, cex=3, add=T)
    plot(focus, pch=1, cex=1.5, col='white', add=T)
    plot(focus, pch=1, cex=1, add=T)
  }
  if(any(grepl('Zones', options))) plot(ref_data$ref_zones, add=T)
  if(any(grepl('Roads', options))) plot(ref_data$ref_roads, add=T, col='red')
  if(any(grepl('Boundary', options))) plot(ref_data$ref_boundary, add=T, lwd=2)
  if(any(grepl('Places', options))){
    plot(ref_data$ref_places$geometry, add=T, pch=16)
    text(st_coordinates(ref_data$ref_places),
         labels = ref_data$ref_places$Name, pos = 1, cex = 1)
  }
}

#' Rasterize IDU shape
#'
#' @param idu_shp
#' @param fields
#' @param res
#'
#' @return
#' @export

rasterize_idu <- function( # parameters
    idu_shp,
    fields = 'IDU_INDEX',
    res = 500
  ){ # function
    idu_rast <- stars::st_rasterize(
      sf = idu_shp |> dplyr::select(all_of(fields)),
      template = stars::st_as_stars(st_bbox(idu_shp), dx=res, dy=res, values=NA_real_))
    return(idu_rast)
}

#' Build IDU raster
#'
#' @param idu_geom
#' @param resolution
#'
#' @return
#' @export

build_idu_raster <- function(idu_geom, resolution){
  r <- terra::rast(terra::vect(idu_geom), res = resolution)
  idu_r <- terra::rasterize(vect(idu_geom), r, 'IDU_INDEX')
  return(idu_r)
}

#' List items in XML file
#'
#' @param idu_xml
#'
#' @return
#' @export

list_pals <- function( # parameters
  idu_xml = 'idu.xml'
){
  idu_xml |> xml2::read_xml() |> xml2::xml_find_all('.//field') |> xml2::xml_attr('col')
}

#' Get palette
#'
#' @param idu_xml
#' @param field
#'
#' @return
#' @export

get_pal <- function(
    field,
    idu_xml = 'idu.xml'
){ # function
  xml <- read_xml(idu_xml)
  xpath_txt <- paste0(".//*[@col='",field,"']")
  pal <- xml2::xml_find_all(xml, xpath_txt) |> xml2::xml_child('attributes') |> xml2::xml_children()
  if(length(pal) > 0){
    pal_df <- purrr::map_dfr(pal, \(x) xml2::xml_attrs(x))
    pal_df <- pal_df |> dplyr::mutate(hex = get_hex(color))
    return(pal_df)
  } else if (length(pal) == 0){
    return(NULL)
  }
}

#' Get hex color
#'
#' @param color
#'
#' @return
#' @export

get_hex <- function( # parameters
  color = NULL
){ # function
  txt <- paste("rgb", color, sep="")
  txt <- gsub(")",", maxColorValue=255)", txt)
  out <- sapply(txt, function(x) eval(parse(text=x))) |> as.character()
  return(out)
}


#' Get label
#'
#' @param field
#' @param value
#'
#' @return
#' @export

get_label <- function(field, value, idu_xml = 'idu.xml'){
  l <- ''
  x <- get_pal(field, idu_xml)
  if(any(names(x)=='minVal')){
    l <- x |>
      dplyr::filter(value >= as.numeric(minVal)) |>
      dplyr::filter(value < as.numeric(maxVal)) |>
      dplyr::pull(label)
  } else {
    l <- x |> dplyr::filter(value == !!value) |> dplyr::pull(label)
    l <- ifelse(length(l) == 0, 'N/A', l)
  }
  if(length(l) == 0) l <- 0
  return(l)
}


#' Title
#'
#' @param field
#' @param value
#'
#' @return
#' @export

lookup_val <- function(field, value, idu_xml = 'idu.xml'){
  pal_df <- get_pal(idu_xml, field = field)
  if(any(names(pal_df) == 'minVal')){
    x <- which(value > pal_df$minVal & value <= pal_df$maxVal)
  } else {
    x = which(value == pal_df$value)
  }
  list(field = field, value = value, label = pal_df$label[x], hex = pal_df$hex[x])
}
