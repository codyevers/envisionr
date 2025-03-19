#' Shiny server wrapper
#'
#' @param ref_data
#' @param datacubes

server_wrapper <- function(ref_data, datacubes, field_filter){

  function(input, output, clientData, session) {

    # [INITIALIZE] -----

    # prepare reference data
    idu_rast <- ref_data$idu_rast
    idu_vect <- ref_data$idu_vect
    roads <- ref_data$ref_roads
    places <- ref_data$ref_places
    zones <- ref_data$ref_zones
    boundary <- ref_data$ref_boundary

    # render the dynamic run label in the UI
    output$run_label <- renderUI({
      h4(strong(paste0('Run ', input$select_run, ' - ', data()$run)))
    })

    # render the dynamic run label in the UI
    output$layer_label <- renderUI({
      h4(paste0(input$select_field, ' in ', input$year))
    })

    # [DATA] -----

    # reactive object to store provided data cubes
    datacube_library <- reactiveValues(datacubes = datacubes)

    # reactive values for storing field information
    fields <- reactiveValues(items = NULL, selected = NULL, initial_selection = NULL)
    fields$initial_selection <- field_filter

    # reactive for year index based on selected year
    year_index <- reactive(input$year - 2018)

    # object to store map brush extent
    ranges <- reactiveValues(x = NULL, y = NULL)

    # reactive values for saving last selected points
    selectedPoint <- reactiveValues(IDU_INDEX = NULL, point = NULL)

    # reactive for map extent
    extent <- reactive({
      if(input$extent == ''){
        txt = '438483.2, 540570.0, 1061211.4, 1149023.9'
        ext <- eval(parse(text = paste0('terra::ext(', txt, ')')))
      } else {
        ext <- eval(parse(text = paste0('terra::ext(', input$extent, ')')))
      }
      return(ext)
    })

    # data reactive helper: retrieve selected datacube
    get_selected_datacube <- function(datacube_library, selected_run) {
        sel <- which(c('A','B','C','D') == selected_run)
        datacube <- datacube_library$datacubes[[sel]]
        return(list(dc = datacube, run_name = attr(datacube, 'run')))
    }

    # data reactive helper: determine year index
    get_year_index <- function(year, count_query) {
      if (count_query) {
        return(NULL) # Count queries don't need a year
      } else {
        return(year - 2018) # Convert year to index
      }
    }

    # data reactive helper: parse extent input
    get_extent <- function(input_extent) {
      if (input_extent == '') {
        txt <- '438483.2, 540570.0, 1061211.4, 1149023.9'
      } else {
        txt <- input_extent
      }
      return(eval(parse(text = paste0('terra::ext(', txt, ')'))))
    }

    # data reactive helper: update UI inputs dynamically
    update_ui_inputs <- function(session, datacube, input) {

      # select all if fields$selected is null
      if(is.null(fields$selected)) {
        fields$selected <- fields$items
      }

      # set default selection if provided in run_envisionr call
      if(!is.null(fields$initial_selection)) {
        fields$selected <- fields$initial_selection
        fields$initial_selection <- NULL
      }

      updateSelectInput(
        session = session,
        inputId = 'select_field',
        choices = fields$selected,
        selected = input$select_field)

      updateSelectInput(
        session = session,
        inputId = "select_run",
        selected = input$select_run)
    }

    # Function to update map raster
    update_map_raster <- function(idu_rast, datacube, field, year, extent, queryA, queryB) {
      out <- NULL
      try({
        out <- update_raster(
          raster = idu_rast,
          datacube = datacube,
          field = field,
          year = year,
          extent = extent,
          queryA = queryA,
          queryB = queryB
        )
      })
      return(out)
    }

    ## data reactive -----
    # input triggers
    # - datacube_library
    # - select_run
    # - select_field
    # - year >> year_index()
    # - extent >> extent()
    # note currently used
    # - county_query
    # - queryA
    # - queryB

    data <- reactive({

      # get the selected datacube
      selected <- get_selected_datacube(datacube_library, input$select_run)
      datacube <- selected$dc
      run_name <- selected$run_name
      fields$items <- names(datacube)

      # determine year index
      y <- get_year_index(input$year, input$count_query)

      # parse spatial extent
      e <- get_extent(input$extent)

      # update UI elements
      update_ui_inputs(session, datacube, input)

      # update the raster map
      idu_rast_updated <- update_map_raster(
        idu_rast = idu_rast,
        datacube = datacube,
        field = input$select_field,
        year = y,
        extent = e,
        queryA = input$queryA,
        queryB = input$queryB)

      # define map palette
      pal_df <- get_pal(input$select_field)
      if (is.null(pal_df)) pal_df <- get_pal('A2rxn')

      return(list(
        run = run_name,
        datacube = datacube,
        idu_rast = idu_rast_updated,
        pal_df = pal_df,
        levels = terra::unique(idu_rast_updated)
      ))
    })

    # [OBSERVER]-----

    # observe map for double clicks
    observe({
      req(input$plot_dblclick)
      x <- input$plot_dblclick$x
      y <- input$plot_dblclick$y
      selectedPoint$IDU_INDEX <- stars::st_extract(
        x = stars::st_as_stars(ref_data$idu_rast),
        at = matrix(c(x, y), ncol=2))

      point_sf <- st_point(c(x, y)) |> st_sfc(crs = 5070)  |> st_sf()
      selectedPoint$point <- point_sf
    })

    # observe for year back button
    observeEvent(input$back, {
      current_year <- input$year
      if (current_year > 2020) {
        updated_year <- current_year - 1
        updateSliderInput(session, "year", value = updated_year)
      }
    })

    # observe for year forward button
    observeEvent(input$forward, {
      current_year <- input$year
      if (current_year < 2059) {
        updated_year <- current_year + 1
        updateSliderInput(session, "year", value = updated_year)
      }
    })

    # select previous attribute
    observeEvent(input$up, {
      choices = fields$selected
      current_index <- match(input$select_field, choices)
      if (current_index > 1) {
        updated_selected <- choices[current_index - 1]
      } else {
        updated_selected <- choices[length(choices)] # Wrap to the last option
      }
      updateSelectizeInput(session, "select_field", selected = updated_selected)
    })

    # select next attribute
    observeEvent(input$down, {
      choices = fields$selected
      current_index <- match(input$select_field, choices)
      if (current_index < length(choices)) {
        updated_selected <- choices[current_index + 1]
      } else {
        updated_selected <- choices[1] # Wrap to the first option
      }
      updated_selected_aux = c(choices[-current_index], choices[current_index])
      updateSelectizeInput(session, "select_field", selected = updated_selected)
      # updateSelectizeInput(session, "fields_aux", selected = updated_selected_aux)
    })

    # select previous run
    observeEvent(input$left, {
      choices = c('A','B','C','D')
      current_index <- match(input$select_run, choices)
      if (current_index > 1) {
        updated_selected <- choices[current_index - 1]
      } else {
        updated_selected <- choices[length(choices)]
      }
      updateSelectInput(session, "select_run", selected = updated_selected)
    })

    # select next run
    observeEvent(input$right, {
      choices = c('A','B','C','D')
      current_index <- match(input$select_run, choices)
      if (current_index < length(choices)) {
        updated_choice <- choices[current_index + 1]
      } else {
        updated_choice <- choices[1]
      }
      updateSelectInput(session, "select_run", selected = updated_choice)
    })

    # reactive value to store selected items in order
    selected_values <- reactiveVal(character(0))

    # observes changes to selectizeInput
    observe({

      # when user selects/deselects manually
      selected <- input$e2
      previous <- selected_values()

      # maintain the order of first selection and only add new selections
      if (!is.null(selected)) {
        new_order <- c(previous[previous %in% selected], setdiff(selected, previous))
      } else {
        new_order <- character(0)
      }

      selected_values(new_order)
    })

    # observe for zoom button press
    observeEvent(input$zoom_button, {
      brush <- input$plot_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        ranges$x[2] <- ranges$x[1] + diff(ranges$y) / 1
        updateTextAreaInput(session, inputId = 'extent', value=paste(
          paste(round(ranges$x,0), collapse=', '),
          paste(round(ranges$y,0), collapse=', '),
          sep=', '))
      }
    })

    # observe zoom reset button
    observeEvent(input$reset_button, {
      updateTextAreaInput(session, inputId = 'extent', value='')
      selectedPoint$IDU_INDEX <- NULL
      selectedPoint$point <- NULL
    })

    # observe show modal button
    observeEvent(input$show, {
      showModal(modalDialog(
        title = "Limit fields to those selected below",
        "Use the dropdown below to select which items to visualize and include in exported data",
        pickerInput(
          inputId = "fields_modal",
          label = "Secondary fields",
          choices =  fields$items,
          selected = fields$selected,
          multiple = TRUE,
          options = pickerOptions(container = "body", actionsBox = TRUE, liveSearch = TRUE),
          width = "100%"
        ),
        easyClose = TRUE,  # Clicking outside the modal will close it
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "Filter")
        )
      ))
    })

    # observe modal OK button
    observeEvent(input$ok, {

      fields$selected <- c(input$fields_modal, input$select_field)

      updateSelectInput(
        session = session,
        inputId = "select_field",
        choices = fields$selected,
        selected = input$select_field
        )

      removeModal()

    })

    # observe changes to selected fields
    observe({
      print(fields$selected)  # Print whenever it updates
    })

    # [RENDER] -----

    ## legend -----

    output$legend <- renderPlot({
      req(data()$levels)

      # continuous palette
      if(any(names(data()$pal_df) == 'minVal')){
        par(mar=rep(0,4), bg = "transparent")
        pal_df <- data()$pal_df
        plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend('topleft', legend = pal_df$label, ncol = 1, fill = pal_df$hex, box.lwd = NA)
      } else { # categorical palette
        par(mar=rep(0,4), bg = "transparent")
        pal_df <- data()$pal_df |> filter(value %in% data()$levels$VALUE)
        plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend('topleft', legend = paste0(pal_df$value, ' ', pal_df$label), ncol = 1, fill = pal_df$hex, box.lwd = NA)
      }
    }, bg = "transparent")

    ## map -----

    output$map <- renderPlot({

      req(input$select_field)
      if(is.null(data()$datacube)) return(NULL)

      pal_field <- input$select_field
      if(input$count_query) {
        pal_field <- NULL
      }

      if(input$queryA != '' | input$queryB != ''){
        pal_df <- data.frame(
          value = c(0,1,10,11),
          hex = c('#FFFFFF','#7AFF62','#847FFF','#FF0000'))
        pal_df <- data.frame(
          value = c(0,1,10,11),
          hex = c('#FFFFFF','purple','purple','black'))
      } else {
        pal_df <- NULL
      }

      plot_raster(
        raster = data()$idu_rast,
        pal_lookup = pal_field,
        pal_table = pal_df,
        extent = extent(),
        focus = selectedPoint$point,
        options = input$plot_options)
    })

    ## attribute -----
    # output$selected_point_info <- renderPrint({
    output$selected_point_info <- renderTable({

      req(selectedPoint$IDU_INDEX)
      fld <- unique(fields$selected)
      idu <- as.numeric(selectedPoint$IDU_INDEX)
      yr <- year_index()

      data_sel <- data()$datacube[fld, idu, yr] |>as.data.frame() |> t()
      out <- data.frame(value = data_sel)

      label <- purrr::map2_chr(rownames(out), out$value, \(x, y){
        pal_x <- get_pal(x)
        label_x <- ''
        try({
          if(is.null(pal_x)) {
            label_x <- ''
          } else {
            if(any(names(pal_x) == 'minVal')){ # binned continous
              label_x <- get_pal('FlameLen') |>
                dplyr::mutate(minVal = as.numeric(minVal)) |>
                dplyr::filter(minVal > as.numeric(y)) |>
                dplyr::slice_head(n = 1) |>
                dplyr::pull(label)
            } else { # categorical
              label_x <- pal_x$label[match(y, pal_x$value)]
            }
          }
          return(label_x)
        })

      })
      out$label <- label
      return(out)
    })

    ## table -----
    output$time_summary <- renderTable({

      req(selectedPoint$IDU_INDEX)

      indx = selectedPoint$IDU_INDEX
      fields <- unique(c(input$select_field, input$fields_aux))

      out <- data.frame(data()$datacube[fields,indx+1,]) |>
        dplyr::select(-idu) |>
        dplyr::filter(year >= input$year - input$time_window) |>
        dplyr::filter(year <= input$year + input$time_window) |>
        dplyr::mutate(year = ifelse(
          year == input$year,
          paste0('**',year, '**'),
          as.character(year)))
    })

    ## save_png  -----
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste(input$select_field,'_','yr',
              input$yr,'_',Sys.time(),
              input$scn.run,'.png', sep='') },
      content = function(file) {
        png(filename = file, width=640, height=480, units = 'px')
        if(is.null(data()$datacube)) return(NULL)

        pal_field <- input$select_field
        if(input$count_query) {
          pal_field <- NULL
        }

        plot_raster(
          raster = data()$idu_rast,
          pal_lookup = pal_field,
          extent = input$extent,
          focus = selectedPoint$point,
          options = input$plot_options)
        dev.off()
      }
    )

    ## save_ani -----
    observeEvent(input$makeAnimation, {

      print('test');

      library(animation)

      saveGIF({
        for(yr in 2:40){

          plot_raster(
            raster = data()$idu_rast,
            pal_lookup = pal_field,
            extent = input$extent,
            focus = selectedPoint$point,
            options = input$plot_options)

          title(main = paste('Year: ', y + 2021))
        }

      }, movie.name = paste0(input$select_field, '.gif'), interval = .5)
    })

    ## save_csv -----

    output$downloadData_fixedSpace <- downloadHandler(
      filename = function() { 'idu_history_output.csv' },
      content = function(file) {
        idu_index <- selectedPoint$IDU_INDEX
        dataset <- data()$datacube[input$fields_aux,idu_index,] |> as.data.frame()
        write.csv(dataset, file, row.names=F)
      }
    )

    output$downloadData_fixedTime <- downloadHandler(
      filename = function() { 'attribute_map_output.csv' },
      content = function(file) {
        dataset <- data()$datacube[input$fields_aux,,year_index()] |> as.data.frame()
        write.csv(dataset, file, row.names=F)
      }
    )

  # load runs panel ----- -----

  # # data ..................
  #
  # shinyDirChoose(input, "dir", roots = c(home = "~/Github/envisionr"))
  #
  # # Reactive to store the selected directory path
  # dirPath <- reactive({
  #   parseDirPath(roots = c(home = "~/Github/envisionr"), input$dir)
  # })
  #
  # # Output the selected directory path
  # output$dirPath <- renderPrint({
  #   dirPath()
  # })
  #
  # # Reactive to get the list of files in the selected directory
  # fileList <- reactive({
  #
  #   # trigger when dirPath updated
  #   path <- dirPath() # < reactive
  #   if (is.null(path)) return(NULL)
  #
  #   # return all datacube files in current directory
  #   files <- list.files(path, full.names = FALSE, pattern = '.datacube')
  #   if (length(files) == 0) return(NULL)
  #
  #   files
  # })
  #
  # # Reactive values to store loaded data
  # loadedData <- reactiveValues(A = NULL, B = NULL, C = NULL, D = NULL)
  #
  # # observers ..................
  #
  # # Update the selectInput choices with first 4 datacube files
  # observe({
  #   files <- fileList() # < reactive
  #   if (is.null(files)) return(NULL)
  #   updateSelectInput(session, "file1", choices = files, selected = files[1])
  #   updateSelectInput(session, "file2", choices = files, selected = files[2])
  #   updateSelectInput(session, "file3", choices = files, selected = files[3])
  #   updateSelectInput(session, "file4", choices = files, selected = files[4])
  # })
  #
  # # observe load runs button
  # observeEvent(input$load_runs, {
  #   path <- dirPath()
  #   if (is.null(path)) {
  #     output$load_status <- renderText("No directory selected.")
  #     return()
  #   }
  #
  #   selected_files <- c(input$file1, input$file2, input$file3, input$file4)
  #   selected_files <- selected_files[!is.na(selected_files) & selected_files != ""]
  #
  #   if (length(selected_files) == 0) {
  #     output$load_status <- renderText("No files selected.")
  #     return()
  #   }
  #
  #   file_paths <- file.path(path, selected_files)
  #
  #   # load files (assuming they are RData or CSV for demonstration)
  #   tryCatch({
  #
  #     load_datacube <- function(x){
  #       load(x)
  #       txt <- paste(attr(datacube, 'run_name'), 'datacube loaded\n')
  #       cat(txt)
  #       output$load_status <- renderText(txt)
  #       return(datacube)
  #     }
  #
  #     datacubes <- list()
  #     if (!is.na(file_paths[1])) datacubes[[1]] <- load_datacube(file_paths[1])
  #     if (!is.na(file_paths[2])) datacubes[[2]] <- load_datacube(file_paths[2])
  #     if (!is.na(file_paths[3])) datacubes[[3]] <- load_datacube(file_paths[3])
  #     if (!is.na(file_paths[4])) datacubes[[4]] <- load_datacube(file_paths[4])
  #     datacube_library$datacubes <- datacubes
  #
  #     output$load_status <- renderText("Files loaded successfully!")
  #   }, error = function(e) {
  #     output$load_status <- renderText(paste("Error loading files:", e$message))
  #   })
  # })
  }
}
