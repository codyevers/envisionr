# ..........................
# UI
# ...........................

# Function to get run names from datacubes
get_run_names <- function(datacubes) {
  default_labels <- c("A", "B", "C", "D") # Default options

  # Extract run names if present, otherwise use default labels
  run_names <- sapply(seq_along(datacubes), function(i) {
    run_name_attr <- attr(datacubes[[i]], 'run_name')
    if (!is.null(run_name_attr)) return(run_name_attr)
    return(default_labels[i])
  })

  # Create a named vector for updating UI
  run_choices <- setNames(default_labels[seq_along(datacubes)], run_names)
  return(run_choices)
}


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

run_envisionr <- function(...) {

  # empty, unnamed list for holding datacubes
  datacubes <- list(...)

  # load reference data
  # if(!exists('ref_data')){
    message('Loading reference data')
    data('ref_data', package = 'envisionr', envir = environment())
  # }

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
