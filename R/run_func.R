
#' Title
#'
#' @param datacubes
#'
#' @return
#' @export
#'
#' @import shiny
#' @import shinyWidgets
#' @import shinyFiles

run_envisionr <- function(..., field_filter = NULL) {

  # empty, unnamed list for holding datacubes
  datacubes <- list(...)

  # load reference data
  if (!exists("ref_data", envir = .GlobalEnv)) {
    message('Loading reference data')
    data('ref_data', package = 'envisionr', envir = .GlobalEnv)
  }

  # names(datacubes) <- get_run_names(datacubes)

  if(file.exists('idu.xml')){
    message('Using idu.xml in working directory')
    idu_xml <- xml2::read_xml('idu.xml')
  } else {
    message('No idu.xml found; adding default included with package')
    idu_xml <- system.file("extdata", "idu.xml", package = "envisionr")
    idu_xml <- xml2::read_xml('data/idu.xml')
    write_xml(idu_xml, file = 'idu.xml')
  }

  if(is.null(datacubes)){
    stop("Datacube(s) required")
  }

  if(length(datacubes) > 4){
    stop("Maximum of 4 runs")
  }

  server <- server_wrapper(ref_data, datacubes, field_filter)

  shinyApp(ui, server)
}
