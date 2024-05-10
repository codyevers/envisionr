# ..........................
# UI
# ...........................

#' Title
#'
#' @param datacubes
#'
#' @return
#' @export
#'
#' @import shiny
#' @import shinyWidgets

run_envisionr <- function(...) {

  datacubes <- list(...)

  if(!exists('ref_data')){
    message('Loading reference data')
    data('ref_data', package = 'envisionr', envir = environment())
  }

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

  server <- server_wrapper(ref_data, datacubes)
  shinyApp(ui, server)
}
